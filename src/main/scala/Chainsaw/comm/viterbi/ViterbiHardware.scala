package Chainsaw.comm.viterbi

import Chainsaw.algos.Metrics.Hamming
import Chainsaw.algos.Trellis
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

import scala.annotation.tailrec
import scala.language.postfixOps

case class ViterbiHardware(trellis: Trellis[Int], length: Int,
                           readAsync: Boolean = true, disWidth: Int = -1) extends Component with DSPTestable[UInt, UInt] {

  // dataTypes and constants
  val outputSymbolType = HardType(UInt(log2Up(trellis.numOutputSymbols) bits))
  val inputSymbolType = HardType(UInt(log2Up(trellis.numInputSymbols) bits))

  val stateWidth = log2Up(trellis.numStates)
  val stateType = HardType(UInt(log2Up(trellis.numStates) bits)) // when biterr is higher, corresponding factor should be higher

  val discrepancyWidth = if (disWidth == -1) log2Up(length / 4) else disWidth // when biterr is higher, corresponding factor should be higher, current config is for biterr up to 5%
  val discrepancyType = HardType(UInt(discrepancyWidth bits)) // when biterr is higher, corresponding factor should be higher

  val incrementWidth = log2Up(log2Up(trellis.numOutputSymbols) + 1)
  val incrementType = HardType(UInt(incrementWidth bits))

  // trellis attributes
  val transitions = trellis.transitions // this represent the structure of hardware connection
  val allStates = 0 until trellis.numStates
  val allOutputs = 0 until trellis.numOutputSymbols

  case class ACS() extends Component {

    val zero = discrepancyType().getZero
    val inf = U(1 << (discrepancyWidth - 1), discrepancyWidth bits) // half of the full range

    // I/O
    val idle = in Bool()
    val dataIn = in(outputSymbolType())
    val dataOut = out Vec(inputSymbolType(), trellis.numStates)

    // build hamming ROMs
    val hammingROMs = allOutputs.map { transitionSymbol =>
      val values = allOutputs
        .map(rxSymbol => Hamming(transitionSymbol, rxSymbol).toInt)
        .map(value => U(value, incrementWidth bits))
      Mem(values)
    }

    // ACS
    val initDis = zero +: Seq.fill(trellis.numStates - 1)(inf)
    val currentDisRegs = initDis.map(RegInit(_)) // this is the only "state" of ACS
    // adding current and increment
    val sums = transitions.map { transition =>
      val inc = hammingROMs(transition.output).readAsync(dataIn) // get hamming distance from ROMs
      currentDisRegs(transition.prevState) + inc
    }
    // updating current by compare & selection
    val min = (a: UInt, b: UInt) => Mux(a < b, a, b)
    val sumSorted = transitions.zip(sums)
      .sortBy(_._1.prevState)
      .sortBy(_._1.nextState)
      .map { pair =>
        pair._2.setName(s"sum${pair._1.prevState}to${pair._1.nextState}")
        pair._2
      }

    @tailrec
    private def getMinWithIndex(data: Seq[UInt]): UInt = {
      val N = data.length
      if (data.length == 1) data.head
      else {
        val (data0, data1) = data.splitAt(N / 2)
        val ret = data0.zip(data1).map { case (left, right) =>
          val bit = right.takeHigh(discrepancyWidth).asUInt < left.takeHigh(discrepancyWidth).asUInt
          Mux(bit, right, left) @@ bit
        }
        getMinWithIndex(ret)
      }
    }

    val mins = sumSorted.grouped(trellis.numInputSymbols).toSeq.map(getMinWithIndex)
    mins.zipWithIndex.foreach { case (min, nextState) =>
      require(min.getBitsWidth == discrepancyWidth + 1)
      val discrepancy = min.takeHigh(discrepancyWidth).asUInt
      val prevStateOrder = min.takeLow(min.getBitsWidth - discrepancyWidth).asUInt
      prevStateOrder.setName(s"orderTo$nextState")
      currentDisRegs(nextState) := Mux(idle, initDis(nextState), discrepancy)
      dataOut(nextState) := RegNext(prevStateOrder)
    }
  }

  case class TB() extends Component {

    val idle = in Bool()
    val dataIn = in Vec(inputSymbolType(), trellis.numStates)
    val dataOut = out(inputSymbolType())

    val currentState = RegInit(stateType().getZero)

    // get prevState and inputSymbol by nextState and order of prevState
    val prevStateValues = Seq.tabulate(trellis.numStates, trellis.numInputSymbols) {
      (nextState, order) =>
        val prevState = trellis.getPrevStatesTo(nextState)(order)
        val input = trellis.getTransition(prevState, nextState).input
        prevState * trellis.numInputSymbols + input
    }.flatten

    // todo: eliminate log2ups
    val prevStateROM = Mem(prevStateValues.map(U(_, stateWidth + log2Up(trellis.numInputSymbols) bits)))
    val order = dataIn(currentState)
    val ret = prevStateROM.readAsync(currentState @@ order)
    val prevState = ret.takeHigh(stateWidth).asUInt
    val inputSymbol = ret.takeLow(log2Up(trellis.numInputSymbols)).asUInt

    // update state
    currentState := Mux(idle, stateType().getZero, prevState)
    dataOut := inputSymbol
  }

  override val dataIn = slave Stream outputSymbolType()
  override val dataOut = master Stream inputSymbolType()
  override val latency = length * 2 + 1 // TODO: verify

  val forward = ACS()
  val backward = TB()
  val recordType = HardType(Bits(log2Up(trellis.numInputSymbols) * trellis.numStates bits))

  val stackOfRecord = DataReverse(recordType, length, readAsync = readAsync)
  val stackOfReverse = DataReverse(inputSymbolType, length)

  dataIn.ready := True
  val periodCounter = Counter(length, inc = dataIn.fire)

  // dataIn -> forward
  forward.dataIn := dataIn.payload

  // forward -> stack, stack -> backward
  forward.idle := periodCounter.willOverflow
  stackOfRecord.dataIn.payload := forward.dataOut.asBits
  stackOfRecord.dataIn.valid := RegNext(dataIn.valid, init = False) // latency of forward = 1

  backward.idle := Delay(periodCounter.willOverflow, if (readAsync) 1 else 2, init = False) // when readSync, extra latency = 1
  stackOfRecord.dataOut.ready := True
  backward.dataIn := Vec(stackOfRecord.dataOut.payload.subdivideIn(inputSymbolType().getBitsWidth bits).map(_.asUInt))

  // backward -> reverse, reverse -> dataOut
  stackOfReverse.dataIn.valid := stackOfRecord.dataOut.valid // when readSync, extra latency = 1
  stackOfReverse.dataIn.payload := backward.dataOut
  stackOfReverse.dataOut >> dataOut
}

