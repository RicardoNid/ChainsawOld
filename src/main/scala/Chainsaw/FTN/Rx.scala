package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class RxPrototype(channelInfo: ChannelInfo) extends Component {

  import channelInfo._

  // modules
  val fft = DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4, 4), Seq(2), fftType, rxUnitType)
  val qamdemod = comm.qam.AdaptiveQamdemod(bitAlloc, powAlloc, rxUnitComplexType)
  val deinterleave = DSP.interleave.AdaptiveMatIntrlv(64, 256, 1024, 1024, HardType(Bool()))

  fft.dataOut.allowOverride
  qamdemod.dataOut.allowOverride

  val dataIn = slave(cloneOf(fft.dataIn))

  // transformations
  def fftPre(in:Vec[SFix]) = Vec(in.zipWithIndex.map{ case (fix, i) => if (i < (bitMask.sum + 1) * 2) fix else rxZero})
  def fftPost(in:Vec[ComplexNumber]) = Vec(in.take(in.length / 2).map(_ >> 9).map(_.truncated(rxUnitType)))

  def bitRemap(in: Bits) = {
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamPositions.indexOf(i)
      if (index == -1) False else bools(qamRemapPositions(index))
    }.reverse.asBits()
  }

  dataIn.t(fftPre) >> fft.dataIn
  fft.dataOut.t(fftPost) >> qamdemod.dataIn
  qamdemod.dataOut.t(bitRemap).t(bits2bools) >> deinterleave.dataIn

}

case class Rx0(channelInfo: ChannelInfo)
  extends RxPrototype(channelInfo) with DSPTestable[Vec[SFix], Vec[ComplexNumber]] {

  override val dataOut = master Stream Vec(rxUnitComplexType(), 256)
  override val latency = fft.latency

  logger.info(s"latency = ${fft.latency}")
  fft.dataOut.t(fftPost) >> dataOut
}

case class Rx1(channelInfo: ChannelInfo)
  extends RxPrototype(channelInfo) with DSPTestable[Vec[SFix], Bits] {

  override val dataOut = master(cloneOf(qamdemod.dataOut))
  override val latency = fft.latency + qamdemod.latency + 2

  qamdemod.dataOut.t(bitRemap) >> dataOut
}

case class Rx2(channelInfo: ChannelInfo)
  extends RxPrototype(channelInfo) with DSPTestable[Vec[SFix], Bits] {

  override val dataOut = master(cloneOf(qamdemod.dataOut))
  override val latency = fft.latency + qamdemod.latency + deinterleave.latency + 2

  deinterleave.dataOut.t(bools2bits) >> dataOut
}

case class Rx3(channelInfo: ChannelInfo)
  extends RxPrototype(channelInfo) with DSPTestable[Vec[SFix], Bits] {

  override val dataOut = master(cloneOf(qamdemod.dataOut))
  override val latency = fft.latency + qamdemod.latency + deinterleave.latency + 2

  deinterleave.dataOut.t(bools2bits) >> dataOut
}