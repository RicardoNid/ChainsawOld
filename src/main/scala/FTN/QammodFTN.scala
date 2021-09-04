package FTN

import Chainsaw._
import matlabIO._
import spinal.core._
import spinal.lib._

// TODO: change this to be dynamic
case class QammodFTN(iter: Boolean) extends Component {

  // TODO: improve the logic
  // fetch parameters from Matlab results
  val pF = if (iter) pFIter * 2 else pFNonIter * 2 // 2 for convenc
  val pFSymbol = pF * 2 / params.BitsPerSymbol // size after hermitian expansion
  val period = params.CarrierNum * params.BitsPerSymbol / pF // period for P2S/S2P
  val bitAlloc = params.bitAlloc
  val powAlloc = params.powAlloc
  val QAM8Symbols = params.SpecialQAM8Symbols

  // I/O
  val inType = HardType(Bits(pF bits))
  val outType = HardType(Vec(qamSymbolType, pFSymbol))
  val dataIn = slave Flow Fragment(inType)
  val dataOut = master Flow Fragment(cloneOf(outType))
  val counterOut = Counter(period)

  // instantiate the core and other components
  val core = Communication.QAMMod(bitAlloc = bitAlloc, powAlloc = powAlloc, symbolType = qamSymbolType)
  printlnGreen(s"qammod instantiated as: period = $period")
  // regs for P2S and S2P, along with their counters
  val serial2parallel = Vec(Reg(inType), period)
  val parallel2serial = Vec(Reg(outType), period)
  val counterIn = Counter(period, inc = dataIn.fire)

  // S2P logic
  when(!dataIn.fire)(counterIn.clear())
  serial2parallel(counterIn.value) := dataIn.payload

  // remap
  //  val flattened: Bits = serial2parallel.reverse.asBits
  //  val extracted = params.QAMPositions.map(i => flattened(params.CarrierNum * params.BitsPerSymbol - i)).toSeq.reverse.asBits

  val extracted = serial2parallel.map { bits =>
    params.DataCarrierPositions.map(i => bits(params.CarrierNum - i)).reverse.toSeq.asBits()
  }.reverse.asBits()

  val headLength = (params.DataCarrierPositions.head - 1) * params.BitsPerSymbol
  val lastLength = (params.CarrierNum - params.DataCarrierPositions.last) * params.BitsPerSymbol
  printlnRed(s"head = $headLength, last = $lastLength")
  val remapped = B(0, headLength bits) ## extracted ## B(0, lastLength bits)

  // connecting the core
  core.dataIn.payload := remapped
  core.dataIn.valid := counterIn.willOverflow

  // P2S logic
  when(core.dataOut.fire)(counterOut.increment())
  when(!(counterOut.value === 0))(counterOut.increment())

  // output & padding
  val masks = params.bitMask.grouped(pFSymbol).toSeq // reshape the masks to have same size as the regs array
  val masked = core.dataOut.payload.zip(params.bitMask).map { case (complex, valid) => if (valid) complex else ComplexNumber(0.0, 0.0, fixedType) }
  val conjed = masked.map(_.conj).reverse
  val hermitianExpanded = Vec(masked ++ conjed)
  //  //  val hermitian = core.dataOut.payload ++ Seq(ComplexNumber(0.0, 0.0, fixedType)) ++ core.dataOut.payload.tail.reverse.map(_.conj)
  //  when(core.dataOut.fire){ // update when new round starts
  //
  //  }
  when(counterOut.willOverflow){
    parallel2serial.zipWithIndex.foreach { case (segment, i) => segment := Vec(hermitianExpanded.slice(i * pFSymbol, (i + 1) * pFSymbol)) }
  }

  dataOut.payload.fragment := parallel2serial(counterOut.value)
  dataOut.valid := core.dataOut.fire || !(counterOut.value === 0)
  // last identify the end of a frame
  // TODO: should I cut the delay of the last data of serial2parallel and parallel2serial?
  val latency = period + 2 // serial2parallel + Qammod + parallel2serial
  printlnGreen(s"logic latency of QAMmod    = $latency")
  //  printlnGreen(s"concrete latency of QAMmod = ${LatencyAnalysis(dataIn.fragment, dataOut.fragment(0).real.raw)}")
  dataOut.last := Delay(dataIn.last, latency, init = False)
}

object QammodFTN extends App {
  GenRTL(new QammodFTN(iter = true))
}
