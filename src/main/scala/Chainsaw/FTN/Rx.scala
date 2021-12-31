package Chainsaw.FTN

import Chainsaw.DSP.S2P
import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib.{Stream, _}

case class RxFront()
  extends Component with DSPTestable[Vec[SFix], Vec[ComplexNumber]] {

  val fft = DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4), Seq(4, 2), fftType, rxUnitType)
  val s2p = S2P(128, 512, fftComplexType)
  val fifo = BigStreamFifo(equalizerComplexVecType, 18)
  val equalizer = EqualizerFTN(preambleSymbols)

  override val dataIn = slave(cloneOf(fft.dataIn))
  override val dataOut = master Stream Vec(rxUnitComplexType, 256)
  override val latency = fft.latency + s2p.latency + equalizer.latency

  def equalizerPost(in: Vec[ComplexNumber]) = Vec(in.map(_.truncated(rxUnitType)))

  def fftPost(in: Vec[ComplexNumber]) =
    Vec(in.take(in.length / 2).map(_ >> 9).map(_.truncated(equalizerType)))

  dataIn >> fft.dataIn
  fft.dataOut >> s2p.dataIn
  s2p.dataOut.t(fftPost) >> fifo.io.push

  // burst transfer
  val burstCounter = Counter(18)
  val inc = fifo.io.occupancy === U(18) || burstCounter.value =/= U(0)
  when(inc)(burstCounter.increment())
  equalizer.dataIn.payload := fifo.io.pop.payload
  equalizer.dataIn.valid := inc
  fifo.io.pop.ready := inc

  equalizer.dataOut.t(equalizerPost) >> dataOut
}

class RxLoop(stage: Seq[Int], actualParallelismOnVit: Int = 512) extends Component {

  import channelInfo._

  val qamdemod = if (stage.contains(0)) comm.qam.AdaptiveQamdemod(bitAlloc, powAlloc, rxUnitComplexType) else null
  val deInterleave = if (stage.contains(0)) DSP.interleave.AdaptiveMatIntrlv(64, 256, 1024, 1024, HardType(Bool())) else null
  // TODO: using same control logic for all vitdecs
  val vitdecs = if (stage.contains(1)) ParallelVitFTN(512, actualParallelismOnVit) else null
  val convencs = if (stage.contains(2)) Convenc512FTN() else null
  val qammod = if (stage.contains(3)) comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType) else null
  val interleave = if (stage.contains(3)) DSP.interleave.AdaptiveMatIntrlv(256, 64, 1024, 1024, HardType(Bool())) else null
  val ifft = if (stage.contains(4)) DSP.FFT.CooleyTukeyHSIFFT(512, Seq(4, 4, 4, 4), Seq(2), ifftType, rxUnitType) else null
  val fft = if (stage.contains(4)) DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4, 4), Seq(2), fftType, rxUnitType) else null

  // transformations
  def fftPre(in: Vec[SFix]) =
    Vec(in.zipWithIndex.map { case (fix, i) => if (i < (bitMask.sum + 1) * 2) fix else rxZero }
      .map(_.truncated(fftType)))

  def fftPost(in: Vec[ComplexNumber]) =
    Vec(in.take(in.length / 2).map(_ >> 9).map(_.truncated(rxUnitType)))

  def bitRemapAfterQamDemod(in: Bits) = {
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamPositions.indexOf(i)
      if (index == -1) False else bools(qamRemapPositions(index))
    }.reverse.asBits()
  }

  def bitRemapBeforeQammod(in: Bits) = {
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamRemapPositions.indexOf(i)
      if (index == -1) False else bools(qamPositions(index))
    }.reverse.asBits()
  }

  def ifftPost(in: Vec[ComplexNumber]) = Vec(in.map(_.real))

  def bitRemapBeforeVitdec(in: Vec[Bool]) = Vec((0 until 1024).map(i =>
    in(i % 2 * 512 + i / 2))
    .grouped(2).toSeq.map(_.reverse.asBits().asUInt)
  )


  def uints2bits(in: Vec[UInt]) = in.reverse.asBits()

  def inside(i: Int)(block: => Unit) = if (stage.contains(i)) block

  def between(i: Int, j: Int)(block: => Unit) = if (stage.contains(i) && stage.contains(j)) block

  // connections
  inside(0)(qamdemod.dataOut.t(bitRemapAfterQamDemod).t(bits2bools) >> deInterleave.dataIn)
  between(0, 1)(deInterleave.dataOut.t(bools2bits) >> vitdecs.dataIn)
  between(1, 2)(vitdecs.dataOut >> convencs.dataIn) // vitdec -> convenc
  between(2, 3)(convencs.dataOut.t(bits2bools) >> interleave.dataIn)
  inside(3)(interleave.dataOut.t(bools2bits).t(bitRemapBeforeQammod) >> qammod.dataIn)
  between(3, 4)(qammod.dataOut.t(doBitMask).t(ifftPre) >> ifft.dataIn)
  inside(4)(ifft.dataOut.t(fftPre) >> fft.dataIn)
}

class Rx0 extends RxLoop(Seq(0))
  with DSPTestable[Vec[ComplexNumber], Bits] {

  val dataIn = slave(cloneOf(qamdemod.dataIn))
  override val dataOut = master(cloneOf(qamdemod.dataOut))
  override val latency = qamdemod.latency + deInterleave.latency

  dataIn >> qamdemod.dataIn
  deInterleave.dataOut.t(bools2bits) >> dataOut
}

class Rx1(actual: Int) extends RxLoop(Seq(1), actual)
  with DSPTestable[Bits, Bits] {

  override val dataIn = slave Stream Bits(1024 bits)
  override val dataOut = master Stream Bits(512 bits)
  override val latency = vitdecs.latency

  dataIn >> vitdecs.dataIn
  vitdecs.dataOut >> dataOut
}

class Rx2 extends RxLoop(Seq(2))
  with DSPTestable[Bits, Bits] {

  override val dataIn = slave Stream Bits(512 bits)
  override val dataOut = master Stream Bits(1024 bits)
  override val latency = convencs.latency

  dataIn >> convencs.dataIn
  convencs.dataOut >> dataOut
}

class Rx3 extends RxLoop(Seq(3))
  with DSPTestable[Bits, Vec[ComplexNumber]] {

  override val dataIn = slave Stream Bits(1024 bits)
  override val dataOut = master(cloneOf(qammod.dataOut))
  override val latency = interleave.latency + qammod.latency

  dataIn.t(bits2bools) >> interleave.dataIn
  qammod.dataOut.t(doBitMask) >> dataOut
}

class Rx4 extends RxLoop(Seq(4)) // it takes 28 min for compilation and 1 s for simulation
  with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  override val dataIn = slave(cloneOf(ifft.dataIn))
  override val dataOut = master Stream Vec(rxUnitComplexType, 256)
  override val latency = ifft.latency + fft.latency

  dataIn.t(ifftPre) >> ifft.dataIn
  fft.dataOut.t(fftPost) >> dataOut
}

class Rx0to2(actual: Int) extends RxLoop(Seq(0, 1, 2), actual)
  with DSPTestable[Vec[ComplexNumber], Bits] {

  override val dataIn = slave Stream Vec(rxUnitComplexType, 256)
  override val dataOut = master Stream Bits(1024 bits)
  override val latency = qamdemod.latency + deInterleave.latency + vitdecs.latency + convencs.latency

  dataIn >> qamdemod.dataIn
  convencs.dataOut >> dataOut
}

class Rx3to4() extends RxLoop(Seq(3,4))
  with DSPTestable[Bits, Vec[ComplexNumber]] {

  override val dataIn = slave Stream Bits(1024 bits)
  override val dataOut = master Stream Vec(rxUnitComplexType, 256)
  override val latency = interleave.latency + qammod.latency  + ifft.latency + fft.latency

  dataIn.t(bits2bools) >> interleave.dataIn
  fft.dataOut.t(fftPost) >> dataOut
}

class RxWhole() extends RxLoop(Seq(0,1,2,3,4))
  with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  override val dataIn = slave Stream Vec(rxUnitComplexType, 256)
  override val dataOut = master Stream Vec(rxUnitComplexType, 256)
  override val latency =
    qamdemod.latency + deInterleave.latency + vitdecs.latency + convencs.latency + interleave.latency + qammod.latency  + ifft.latency + fft.latency

  dataIn >> qamdemod.dataIn
  fft.dataOut.t(fftPost) >> dataOut
}