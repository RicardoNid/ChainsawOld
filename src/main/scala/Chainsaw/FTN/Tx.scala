package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

class Tx(implicit ftnParams: FtnParams)
  extends Component {

  import ftnParams.channelInfo._

  // components
  val convenc = Convenc128FTN()
  val interleave = DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool()))
  val s2p0 = DSP.S2P(256, 1024, HardType(Bool()))
  val qammod = comm.qam.AdaptiveQammod(bitAlloc, powAlloc, symbolType)
  val p2s0 = DSP.P2S(512, 128, symbolComplexType)
  val ifft = DSP.FFT.CooleyTukeyHSIFFT(
    N = 512, pF = 128,
    dataType = symbolType, coeffType = coeffType,
    factors = Seq(4, 4, 4, 4, 2), shifts = ifftShifts)
  val s2p1 = DSP.S2P(128, 512, ifftType)
  val p2s1 = DSP.P2S(512, 128, ifftType)

  // connections and transformations
  convenc.dataOut.payloadMap(bits2bools) >> interleave.dataIn
  interleave.dataOut >> s2p0.dataIn
  s2p0.dataOut.payloadMap(bools2bits).payloadMap(bitRemapBeforeQammod) >> qammod.dataIn
  qammod.dataOut.payloadMap(doBitMask).payloadMap(ifftPre) >> p2s0.dataIn
  p2s0.dataOut >> ifft.dataIn
  ifft.dataOut.m2sPipe() >> s2p1.dataIn
  s2p1.dataOut.payloadMap(ifftPost) >> p2s1.dataIn

  def doTxScaling(in: Vec[SFix]) = {
    val upper = ftnParams.rangeLimit
    val lower = -ftnParams.rangeLimit
    Vec(in.map { fix =>
      val int = SInt(7 bits) // [-128, 127]
      int assignFromBits fix.asBits.takeHigh(7)
      val ret = SInt(6 bits)
      when(int > S(upper))(ret := S(upper))
        .elsewhen(int < S(lower))(ret := S(lower))
        .otherwise(ret assignFromBits int.takeLow(6))
      (~ret.msb ## ret.takeLow(5)).asUInt // equals to +32
    })
  }
}

case class Tx0(implicit ftnParams: FtnParams)
  extends Tx() with DSPTestable[Bits, Bits] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Bits(256 bits)
  override val latency = Seq(convenc, interleave).map(_.latency).sum

  dataIn >> convenc.dataIn
  interleave.dataOut.allowOverride
  interleave.dataOut.payloadMap(bools2bits) >> dataOut
}

case class Tx1(implicit ftnParams: FtnParams)
  extends Tx() with DSPTestable[Bits, Vec[ComplexNumber]] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(symbolComplexType, 256)
  override val latency = Seq(convenc, interleave, s2p0, qammod).map(_.latency).sum

  dataIn >> convenc.dataIn
  qammod.dataOut.allowOverride
  qammod.dataOut.payloadMap(doBitMask) >> dataOut
}

case class Tx2(implicit ftnParams: FtnParams)
  extends Tx() with DSPTestable[Bits, Vec[SFix]] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(ifftType(), 128)
  override val latency = Seq(convenc, interleave, s2p0, qammod, p2s0, ifft).map(_.latency).sum

  dataIn >> convenc.dataIn
  ifft.dataOut.allowOverride
  ifft.dataOut >> dataOut
  logger.info(s"Tx generated, latency = $latency")
}

case class TxWhole(implicit ftnParams: FtnParams)
  extends Tx() with DSPTestable[Bits, Vec[UInt]] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(UInt(6 bits), 128)
  override val latency = Seq(convenc, interleave, s2p0, qammod, p2s0, ifft).map(_.latency).sum

  dataIn >> convenc.dataIn
  p2s1.dataOut.payloadMap(doTxScaling) >> dataOut
  logger.info(s"Tx generated, latency = $latency")
}