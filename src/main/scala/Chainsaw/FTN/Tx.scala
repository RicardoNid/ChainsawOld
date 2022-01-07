package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

class Tx(channelInfo: ChannelInfo)
  extends Component {

  import channelInfo._

  def bitRemap(in: Bits) = {
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamRemapPositions.indexOf(i)
      if (index == -1) False else bools(qamPositions(index))
    }.reverse.asBits()
  }

  // definitions of modules
  val convenc = Convenc128FTN()
  val interleave = DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool()))
  val s2p = DSP.S2P(256, 1024, HardType(Bool()))
  val qammod = comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType)
  val p2s = DSP.P2S(512, 128, unitComplexType)
  //  cmultConfig = ComplexMultConfig(true, 3, ifftType)
  val (shifts1, shifts2) = ifftShifts.splitAt(3)
  val ifft = DSP.FFT.CooleyTukeyHSIFFT(
    N = 512,
    factors1 = Seq(4, 4, 4), factors2 = Seq(4, 2),
    dataType = unitType, coeffType = unitType,
    shifts1 = shifts1, shifts2 = shifts2)

  // connecting modules and transformations
  convenc.dataOut.t(bits2bools) >> interleave.dataIn
  interleave.dataOut >> s2p.dataIn
  s2p.dataOut.t(bools2bits).t(bitRemap) >> qammod.dataIn
  qammod.dataOut.t(doBitMask).t(ifftPre) >> p2s.dataIn
  //  p2s.dataOut.t(doVecTrunc(_, ifftType)) >> ifft.dataIn
  p2s.dataOut >> ifft.dataIn

  // TODO: register for scaling
  def doScaling(in: Vec[SFix]) = {
    Vec(in.map { fix =>
      val int = SInt(7 bits) // [-128, 127]
      int assignFromBits fix.asBits.takeHigh(7)
      val ret = SInt(6 bits)
      when(int > S(31))(ret := S(31))
        .elsewhen(int < S(-32))(ret := S(-32))
        .otherwise(ret assignFromBits int.takeLow(6))
      ret
    })
  }

}

case class Tx0(channelInfo: ChannelInfo)
  extends Tx(channelInfo) with DSPTestable[Bits, Bits] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Bits(256 bits)
  override val latency = Seq(convenc, interleave).map(_.latency).sum

  dataIn >> convenc.dataIn
  interleave.dataOut.allowOverride
  interleave.dataOut.t(bools2bits) >> dataOut
}

case class Tx1(channelInfo: ChannelInfo)
  extends Tx(channelInfo) with DSPTestable[Bits, Vec[ComplexNumber]] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(unitComplexType, 256)
  override val latency = Seq(convenc, interleave, s2p, qammod).map(_.latency).sum

  dataIn >> convenc.dataIn
  qammod.dataOut.allowOverride
  qammod.dataOut.t(doBitMask) >> dataOut
}

case class Tx2(channelInfo: ChannelInfo)
  extends Tx(channelInfo) with DSPTestable[Bits, Vec[SFix]] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(ifftType(), 128)
  override val latency = Seq(convenc, interleave, s2p, qammod, p2s, ifft).map(_.latency).sum

  dataIn >> convenc.dataIn
  ifft.dataOut >> dataOut
  logger.info(s"Tx generated, latency = $latency")
}

case class TxWhole(channelInfo: ChannelInfo)
  extends Tx(channelInfo) with DSPTestable[Bits, Vec[SInt]] {

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(SInt(6 bits), 128)
  override val latency = Seq(convenc, interleave, s2p, qammod, p2s, ifft).map(_.latency).sum

  dataIn >> convenc.dataIn
  ifft.dataOut.t(doScaling) >> dataOut
  logger.info(s"Tx generated, latency = $latency")
}