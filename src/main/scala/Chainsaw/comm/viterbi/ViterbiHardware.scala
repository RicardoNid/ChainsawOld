package Chainsaw.comm.viterbi

import Chainsaw.algos.Metrics.Hamming
import Chainsaw.algos.Trellis
import Chainsaw.dspTest.DSPTestable
import Chainsaw.logger
import spinal.core._
import spinal.lib.{Counter, Delay, master, slave}

import scala.annotation.tailrec

/**
 * @param trellis   convolution code definition
 * @param length    fixed length of
 * @param readAsync readAsync/readSync
 * @param disWidth  width of discrepancy
 * @see we adopt the terminologies in ''code theory''
 */
// TODO: share hamming ROM and prevState ROM?
case class ViterbiHardware(trellis: Trellis[Int], length: Int, copies: Int = 1,
                           readAsync: Boolean = true, disWidth: Int = -1)
  extends Component with DSPTestable[Vec[UInt], Vec[UInt]] {

  // dataTypes and constants
  val outputSymbolType = HardType(UInt(log2Up(trellis.numOutputSymbols) bits))
  val inputWidth = log2Up(trellis.numInputSymbols)
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

  val hammingROMs = allOutputs.map { transitionSymbol =>
    val value = allOutputs
      .map(rxSymbol => Hamming(transitionSymbol, rxSymbol).toInt)
      .map(value => U(value, incrementWidth bits))
    Mem(value)
  }

  case class ACS() extends Component {

    val zero = discrepancyType().getZero
    val inf = U(1 << (discrepancyWidth - 1), discrepancyWidth bits) // half of the full range

    // I/O
    val idle = in Bool()
    val dataIn = in(outputSymbolType())
    val dataOut = out Vec(inputSymbolType(), trellis.numStates)
    val hammingIn = in Vec(UInt(incrementWidth bits), trellis.numOutputSymbols)

    // ACS
    val initDis = zero +: Seq.fill(trellis.numStates - 1)(inf)
    //    val currentDisRegs = initDis.map(RegInit(_)) // this is the only "state" of ACS
    val currentDisRegs = Seq.fill(trellis.numStates)(Reg(UInt(discrepancyWidth bits))) // this is the only "state" of ACS
    // adding current and increment
    val sums = transitions.map { transition =>
      val inc = hammingIn(transition.output) // get hamming distance from ROMs
      currentDisRegs(transition.prevState) + inc
    }
    // updating current by compare & selection
    val sumSorted: Seq[UInt] = transitions.zip(sums)
      .sortBy(_._1.prevState) // second important
      .sortBy(_._1.nextState) // most important
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

    val mins = sumSorted.grouped(trellis.numInputSymbols) // groups of transitions to same states
      .toSeq.map(getMinWithIndex)

    val reduce1 = mins.map(_.msb).reduce(_ && _) // when all discrepancies has a valid msb
    val reduce2 = mins.map(min => min.takeHigh(2).orR).reduce(_ && _)

    def reduceLogic1(reduce: Bool, value: UInt) =
      Mux(reduce, False, value.msb).asUInt @@ value.takeLow(value.getBitsWidth - 1).asUInt

    def reduceLogic2(reduce: Bool, value: UInt) = {
      val high = UInt(2 bits)
      when(reduce && value.takeHigh(2) === B"11")(high := U(2, 2 bits))
        .elsewhen(reduce && value.takeHigh(2) === B"10")(high := U(1, 2 bits))
        .elsewhen(reduce && value.takeHigh(2) === B"01")(high := U(0, 2 bits))
        .otherwise(high := value.takeHigh(2).asUInt)
      high @@ value.takeLow(value.getBitsWidth - 2).asUInt
    }

    val useReduction = 2

    mins.zipWithIndex.foreach { case (min, nextState) =>
      require(min.getBitsWidth == discrepancyWidth + inputWidth)
      val temp = min.takeHigh(discrepancyWidth).asUInt
      val discrepancy =
        if (useReduction == 2) reduceLogic2(reduce2, temp)
        else if (useReduction == 1) reduceLogic1(reduce1, temp)
        else temp
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

    val currentState = Reg(stateType().getZero)
    val order = dataIn(currentState)

    val prevState = currentState.takeLow(stateWidth - inputWidth).asUInt @@ order
    val inputSymbol = currentState.takeHigh(inputWidth).asUInt

    // update state
    currentState := Mux(idle, stateType().getZero, prevState)
    dataOut := inputSymbol
  }

  override val dataIn = slave Stream Vec(outputSymbolType(), copies)
  override val dataOut = master Stream Vec(inputSymbolType(), copies)
  override val latency = length * 2 + (if (readAsync) 1 else 3)
  logger.info(s"implementing a vitdec at of length $length containing $copies copies, latency = $latency")

  val forwards = Seq.fill(copies)(ACS())
  val backwards = Seq.fill(copies)(TB())
  val recordType = HardType(Bits(log2Up(trellis.numInputSymbols) * trellis.numStates bits))

  val stackOfRecords = Seq.fill(copies)(DataReverse(recordType, length, readAsync = readAsync))
  val stackOfReverse = DataReverse(Vec(inputSymbolType(), copies), length, readAsync = readAsync)

  dataIn.ready := True
  val periodCounter = Counter(length, inc = dataIn.fire)

  // dataIn -> forward
  forwards.zip(dataIn.payload).foreach { case (acs, int) =>
    acs.dataIn := int
    acs.hammingIn := Vec(hammingROMs.map(_.readAsync(int)))
  }

  // forward -> stack, stack -> backward
  val acsIdle = periodCounter.valueNext === U(0)
  val tbIdle = Delay(acsIdle.rise(), if (readAsync) 1 else 2, init = False)
  val acsInReady = RegNext(dataIn.valid, init = False)
  forwards.zip(backwards).zip(stackOfRecords).foreach {
    case ((acs, tb), stack) =>
      acs.idle := acsIdle
      stack.dataIn.payload := acs.dataOut.asBits
      stack.dataIn.valid := acsInReady // latency of forward = 1

      tb.idle := tbIdle
      tb.dataIn := Vec(stack.dataOut.payload.subdivideIn(inputSymbolType().getBitsWidth bits).map(_.asUInt))
      stack.dataOut.ready := True
  }

  // backward -> reverse, reverse -> dataOut
  stackOfReverse.dataIn.valid := stackOfRecords.head.dataOut.valid // when readSync, extra latency = 1
  stackOfReverse.dataIn.payload := Vec(backwards.map(_.dataOut))
  stackOfReverse.dataOut >> dataOut
}
