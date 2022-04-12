package Chainsaw.DSP.DAS
import spinal.core._
import spinal.lib._

case class RowDiffAndUnwrap() extends Component {
  val io = new Bundle {
    val dataIn  = slave Flow (Para.dataType())
    val dataOut = master Flow (Para.dataType())
  }

  // row diff
  val shiftRegLoadCounter = Counter(0, Para.gauge)
  when(io.dataIn.valid && ~shiftRegLoadCounter.willOverflowIfInc)(shiftRegLoadCounter.increment())
  val shiftRegs = Vec(Reg(Para.dataType()) init (0), Para.gauge)
  shiftRegs.init.zip(shiftRegs.tail).foreach { case (i, t) => when(io.dataIn.valid)(t := i) }
  when(io.dataIn.valid)(shiftRegs.head := io.dataIn.payload)

  val xn = shiftRegs.last - io.dataIn.payload

  val xnFlow = Flow(Para.dataType)
  xnFlow.payload := xn
  xnFlow.valid   := shiftRegLoadCounter.willOverflowIfInc & io.dataIn.valid

  val firstUnwrap = Unwrap(Para.rowCount, xnFlow)

  // acc and average
  val counter = Counter(0, Para.gauge - 1)
  when(firstUnwrap.io.xnOut.valid)(counter.increment())
  val acc     = Reg(Para.dataType()) init (0)
  val divisor = Para.dataType()
  divisor := 1 / Para.gauge.toDouble

  val xnFlow2 = Flow(Para.dataType())
  xnFlow2.payload := 0
  xnFlow2.valid.clear()
  when(counter.willOverflow) {
    acc := 0
    xnFlow2.valid.set()
    xnFlow2.payload := (acc * divisor).truncated
  } elsewhen (firstUnwrap.io.xnOut.valid)(acc := acc + firstUnwrap.io.xnOut.payload)

  val secondUnwrap = Unwrap(Para.rowCount2, xnFlow2)

  io.dataOut := secondUnwrap.io.xnOut
}
