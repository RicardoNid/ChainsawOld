package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class Tx(channelInfo: ChannelInfo)
  extends Component with DSPTestable[Bits, Vec[SFix]] {

  import channelInfo._

  def bitRemap(in: Bits) = {
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamRemapPositions.indexOf(i)
      if (index == -1) False else bools(qamPositions(index))
    }.reverse.asBits()
  }

  def ifftPost(in: Vec[ComplexNumber]) = Vec(in.map(_.real))

  // definitions of modules
  val convenc = Convenc128FTN()
  val interleave = DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool()))
  val s2p = DSP.S2P(256, 1024, HardType(Bool()))
  val qammod = comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType)
  val p2s = DSP.P2S(512, 128, ifftComplexType)
  //  val ifft = DSP.FFT.CooleyTukeyBackToBack(512, 128, Seq(4,4,4,2), Seq(4), true, ifftType, unitType)
  val ifft = DSP.FFT.CooleyTukeyHSIFFT(512, Seq(4, 4, 4), Seq(4, 2), ifftType, unitType)

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(ifftType(), 128)
  override val latency = Seq(convenc, interleave, s2p, qammod, p2s, ifft).map(_.latency).sum

  // connecting modules and transformations
  dataIn >> convenc.dataIn
  convenc.dataOut.t(bits2bools) >> interleave.dataIn
  interleave.dataOut >> s2p.dataIn
  s2p.dataOut.t(bools2bits).t(bitRemap) >> qammod.dataIn
  qammod.dataOut.t(doBitMask).t(ifftPre) >> p2s.dataIn
  p2s.dataOut >> ifft.dataIn
  //  ifft.dataOut.t(ifftPost) >> dataOut
  ifft.dataOut >> dataOut

  logger.info(s"Tx generated, latency = $latency")
}