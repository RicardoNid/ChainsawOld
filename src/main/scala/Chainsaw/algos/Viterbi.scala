package Chainsaw.algos

import Chainsaw._
import breeze.linalg._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object TerminationMode extends Enumeration {
  type TerminationMode = Value
  val TRUNCATION, TERMINATION = Value
}

import Chainsaw.algos.TerminationMode._

object Viterbi {

  /** plain implementation of general Viterbi algorithm by maintaining paths
   *
   * @param rxSymbols received sequence of output symbols with noise
   * @param trellis   trellis with input symbol type: Int and output symbol type: T
   * @param metric    metric that defines the distance between an expected output symbol and an observed output symbol
   * @param mode      termination mode
   * @tparam T type of output symbols, could be int(hard) or double(soft)
   * @see ''Coding Theory'' Chap3.1.4 for termination mode, 3.2.3 for Viterbi algorithm
   */
  @definition
  def viterbi[T](rxSymbols: DenseVector[T],
                 trellis: Trellis[T],
                 metric: (T, T) => Double,
                 mode: TerminationMode = TERMINATION)(implicit tag: ClassTag[T]): DenseVector[Int] = {

    // the path we maintain
    case class Path(states: Seq[Int], inputs: Seq[Int], var discrepancy: Double) {

      def expand(nextState: Int, nextInput: Int, increment: Double) =
        Path(states :+ nextState, inputs :+ nextInput, discrepancy + increment)

      override def toString = states.mkString(" -> ") + s" $discrepancy"
    }

    val allStates = 0 until trellis.numStates
    // initializing paths to all states
    val inf = Int.MaxValue
    var paths = allStates.map {
      case 0 => Path(Seq(0), Seq[Int](), 0)
      case i => Path(Seq(i), Seq[Int](), inf) // as they're invalid
    }

    // iterating on received symbols
    val rxQueue = mutable.Queue(rxSymbols.toArray: _*)
    while (rxQueue.nonEmpty) {
      val currentSymbol = rxQueue.dequeue()
      // transitions to all states
      val transitionsToAllStates = allStates.map(trellis.getTransitionsTo)
      // update paths
      paths = transitionsToAllStates.map { transitionsToOneState =>
        // find currently shortest path to each states
        val minTransition = transitionsToOneState.minBy { transition =>
          paths(transition.prevState).discrepancy + metric(transition.output, currentSymbol)
        }
        // update
        paths(minTransition.prevState).expand(
          minTransition.nextState,
          minTransition.input,
          metric(minTransition.output, currentSymbol))
      }
    }

    val txSymbols = mode match {
      case TRUNCATION => paths.minBy(_.discrepancy).inputs
      case TERMINATION => paths.head.inputs
    }

    new DenseVector(txSymbols.toArray)
  }

  /** implementation of general Viterbi algorithm by tracing back rather than maintaining paths
   */
  @hardAlgo("viterbi")
  def viterbiTraceback[T](rxSymbols: DenseVector[T],
                          trellis: Trellis[T],
                          metric: (T, T) => Double,
                          mode: TerminationMode = TERMINATION)(implicit tag: ClassTag[T]) = {

    val allStates = 0 until trellis.numStates
    // we maintain the discrepancies only, not all information of paths(states and inputs)
    val inf = 8
    var currentMinimums: Seq[Double] = 0.0 +: Seq.fill(trellis.numStates - 1)(inf.toDouble)
    val selectionRecords = allStates.map(pair => pair -> mutable.Stack[Int]()).toMap

    val disRange = ArrayBuffer[Double]()
    val numbersOfMin = ArrayBuffer[Int]()

    // forwarding: iterating on received symbols and build records
    val rxQueue = mutable.Queue(rxSymbols.toArray: _*)
    while (rxQueue.nonEmpty) {
      val currentSymbol = rxQueue.dequeue()
      // transitions to all states
      val transitionsToAllStates = allStates.map(trellis.getTransitionsTo)
      // add, compare and select(ACS)
      val newDis = transitionsToAllStates.map { transitionsToOneState =>
        val discrepanciesOneState = transitionsToOneState.map { transition =>
          val current = currentMinimums(transition.prevState)
          val increment = metric(transition.output, currentSymbol)
          // record discrepancies on each transitions for tracing back
          current + increment
        }
        numbersOfMin += discrepanciesOneState.count(_ == discrepanciesOneState.min)
        val minTransition = discrepanciesOneState.zip(transitionsToOneState).sortBy(_._2.input).minBy(_._1)._2
        selectionRecords(minTransition.nextState).push(minTransition.prevState) // in hardware, we only record the branch
        discrepanciesOneState.min
      }
      currentMinimums = newDis
      disRange += currentMinimums.max - currentMinimums.min
      if (currentMinimums.forall(value => value >= 16 && value < 32)) currentMinimums = currentMinimums.map(_ - 16.0)
    }
    //    println(s"max in records: ${currentMinimums.max}")
    //    println(s"min in records: ${currentMinimums.min}")
    //    println(s"disRange max: ${disRange.max}")

    // tracing back

    var traceBackState = mode match {
      case TRUNCATION => currentMinimums.indexWhere(_ == currentMinimums.min)
      case TERMINATION => 0
    }

    val traceBackStates = ArrayBuffer[Int](traceBackState)

    val txSymbols = mutable.Queue[Int]()
    while (selectionRecords.values.head.nonEmpty) { // the first record shouldn't produce result(no transition to that)
      val prevState = selectionRecords(traceBackState).top
      txSymbols.enqueue(trellis.getTransition(prevState, traceBackState).input)
      traceBackState = selectionRecords(traceBackState).top
      traceBackStates += traceBackState
      selectionRecords.values.foreach(_.pop())
    }

    //    println(s"states: ${traceBackStates.reverse.map(_.toString.padTo(2, ' ')).mkString("<-")}")
    new DenseVector(txSymbols.reverse.toArray)
  }
}
