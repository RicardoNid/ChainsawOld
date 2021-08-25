package FTN

import Chainsaw._
import matlabIO._
import spinal.core._
import spinal.lib._

// TODO: change this to be dynamic
case class  QammodFTN(bitAlloc: Seq[Int], powAlloc: Seq[Double], period: Int) extends Component {

  val symbolsFor3 = Seq(new MComplex(1, 1), new MComplex(1, -1))
  val core = Communication.QAMMod(bitAlloc = bitAlloc, powAlloc = powAlloc, symbolType = complexType, customSymbols = Map(3 -> symbolsFor3))

  val symbolCount = bitAlloc.size / period
  val inType = HardType(Bits(bitAlloc.sum / period bits))
  val outType = HardType(Vec(complexType, symbolCount))

  val dataIn = slave Flow Fragment(inType)
  val dataOut = master Flow Fragment(cloneOf(outType))

  // input logic
  val counterIn = Counter(period, inc = dataIn.fire)
  when(!dataIn.fire)(counterIn.clear())

  val serial2parallel = Vec(Reg(inType), period)
  serial2parallel(counterIn.value) := dataIn.payload

  core.dataIn.payload := serial2parallel.reverse.asBits
  core.dataIn.valid := counterIn.willOverflow

  // output logic
  val counterOut = Counter(period)
  when(core.dataOut.fire)(counterOut.increment())
  when(!(counterOut.value === 0))(counterOut.increment())

  val parallel2serial = Vec(Reg(outType), period)
  // core.dataOut >> parallel2serial
  parallel2serial.zipWithIndex.foreach { case (segment, i) => segment := Vec(core.dataOut.payload.slice(i * symbolCount, (i + 1) * symbolCount)) }

  dataOut.payload.fragment := parallel2serial(counterOut.value)
  dataOut.valid := core.dataOut.fire || !(counterOut.value === 0)
  // last identify the end of a frame
  // TODO: should I cut the delay of the last data of serial2parallel and parallel2serial?
  val latency = period // serial2parallel + Qammod + parallel2serial
//  printlnGreen(s"latency of QAMmod = $latency")
  printlnGreen(s"latency of QAMmod = ${LatencyAnalysis(dataIn.fragment, dataOut.fragment(0).real.raw)}")
  dataOut.last := Delay(dataIn.last, latency, init = False)
}

object QammodFTN extends App {
  val bitAlloc = Array.fill(16)(4)
  val powAlloc = Array.fill(16)(1.0)
  GenRTL(new QammodFTN(bitAlloc, powAlloc, period = 8))
}
