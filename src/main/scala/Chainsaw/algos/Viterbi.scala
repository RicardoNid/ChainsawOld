package Chainsaw.algos

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.Queue
import scala.reflect.ClassTag

object TerminationMode extends Enumeration {
  type TerminationMode = Value
  val TRUNCATION, TERMINATION, TAILBITING = Value
}

import TerminationMode._

object Viterbi {

  def bits2Symbols(bits: DenseVector[Int], symbolSize: Int) = {
    val symbols: Array[Int] = bits.toArray.grouped(symbolSize).toArray
      .map(_.mkString("")).map(BigInt(_, 2).toInt)
    new DenseVector(symbols)
  }

  def symbols2Bits(symbols: DenseVector[Int], symbolSize: Int) = {
    val bits = symbols.toArray.flatMap(_.toBinaryString.padToLeft(symbolSize, '0').map(_.asDigit))
    new DenseVector(bits)
  }

  def viterbi[T](rxSymbols: DenseVector[T],
                 trellis: Trellis[T],
                 metric: (T, T) => Double,
                 mode: TerminationMode = TERMINATION)(implicit tag: ClassTag[T]): DenseVector[Int] = {

    case class Path(states: Seq[Int], inputs: Seq[Int], var discrepancy: Double) {

      def expand(nextState: Int, nextInput: Int, increment: Double) =
        Path(states :+ nextState, inputs :+ nextInput, discrepancy + increment)

      override def toString() = states.mkString(" -> ") + s" $discrepancy"
    }

    val allStates = 0 until trellis.numStates
    // maintaining paths to all states
    var paths = allStates.map {
      case 0 => Path(Seq(0), Seq[Int](), 0)
      case i => Path(Seq(i), Seq[Int](), Int.MaxValue) // as they're invalid
    }

    // iterating on queues
    val rxQueue = Queue(rxSymbols.toArray: _*)
    while (rxQueue.nonEmpty) {
      val currentSymbol = rxQueue.dequeue()
      // transitions to all states
      val transitionsAllStates = allStates.map(trellis.getTransitionsTo)
      // update paths
      paths = transitionsAllStates.map { transitionsOneState =>
        // find min
        val minTransition = transitionsOneState.minBy { transition =>
          paths(transition.prevState).discrepancy + metric(transition.output, currentSymbol)
        }

        paths(minTransition.prevState).expand(
          minTransition.nextState,
          minTransition.input,
          metric(minTransition.output, currentSymbol))
      }
    }

    val txSymbols = mode match {
      case TRUNCATION => paths.minBy(_.discrepancy).inputs
      case TERMINATION => paths.head.inputs
      case TAILBITING => throw new IllegalArgumentException("not implemented yet")
    }

    new DenseVector(txSymbols.toArray)
  }
}
