package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class RxLoop(stage: Seq[Int], actualParallelismOnVit: Int = 512) extends Component {

  import channelInfo._

  val qamdemod = if (stage.contains(0)) comm.qam.AdaptiveQamdemod(bitAlloc, powAlloc, symbolComplexType) else null
  val deInterleave = if (stage.contains(0)) DSP.interleave.AdaptiveMatIntrlv(64, 256, 1024, 1024, HardType(Bool())) else null
  // TODO: using same control logic for all vitdecs
  val vitdecs = if (stage.contains(1)) ParallelVitFTN(512, actualParallelismOnVit) else null
  val convencs = if (stage.contains(2)) Convenc512FTN() else null
  val qammod = if (stage.contains(3)) comm.qam.AdaptiveQammod(bitAlloc, powAlloc, symbolType) else null
  val interleave = if (stage.contains(3)) DSP.interleave.AdaptiveMatIntrlv(256, 64, 1024, 1024, HardType(Bool())) else null
  val ifft = if (stage.contains(4)) DSP.FFT.CooleyTukeyHSIFFT(512, 512, symbolType, coeffType, Seq(4, 4, 4, 4, 2), ifftShifts) else null
  val fft = if (stage.contains(4)) DSP.FFT.CooleyTukeyRVFFT(512, 512, ifftType, coeffType, Seq(4, 4, 4, 4, 2), fftShifts) else null

  // transformations

  def fftPost(in: Vec[ComplexNumber]) =
    Vec(in.take(in.length / 2).map(_ >> 9).map(_.truncated(symbolType)))

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

  def bitRemapBeforeVitdec(in: Vec[Bool]) = Vec((0 until 1024).map(i =>
    in(i % 2 * 512 + i / 2))
    .grouped(2).toSeq.map(_.reverse.asBits().asUInt)
  )

  def uints2bits(in: Vec[UInt]) = in.reverse.asBits()

  def inside(i: Int)(block: => Unit) = if (stage.contains(i)) block

  def between(i: Int, j: Int)(block: => Unit) = if (stage.contains(i) && stage.contains(j)) block

  // connections
  inside(0)(qamdemod.dataOut.payloadMap(bitRemapAfterQamDemod).payloadMap(bits2bools) >> deInterleave.dataIn)
  between(0, 1)(deInterleave.dataOut.payloadMap(bools2bits) >> vitdecs.dataIn)
  between(1, 2)(vitdecs.dataOut >> convencs.dataIn) // vitdec -> convenc
  between(2, 3)(convencs.dataOut.payloadMap(bits2bools) >> interleave.dataIn)
  inside(3)(interleave.dataOut.payloadMap(bools2bits).payloadMap(bitRemapBeforeQammod) >> qammod.dataIn)
  between(3, 4)(qammod.dataOut.payloadMap(doBitMask).payloadMap(ifftPre) >> ifft.dataIn)
  inside(4)(ifft.dataOut.payloadMap(ifftPost) >> fft.dataIn)
}

class Rx0 extends RxLoop(Seq(0))
  with DSPTestable[Vec[ComplexNumber], Bits] {

  val dataIn = slave(cloneOf(qamdemod.dataIn))
  override val dataOut = master(cloneOf(qamdemod.dataOut))
  override val latency = qamdemod.latency + deInterleave.latency

  dataIn >> qamdemod.dataIn
  deInterleave.dataOut.payloadMap(bools2bits) >> dataOut
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

  dataIn.payloadMap(bits2bools) >> interleave.dataIn
  qammod.dataOut.payloadMap(doBitMask) >> dataOut
}

class Rx4 extends RxLoop(Seq(4)) // it takes 28 min for compilation and 1 s for simulation
  with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  override val dataIn = slave(cloneOf(ifft.dataIn))
  override val dataOut = master Stream Vec(symbolComplexType, 256)
  override val latency = ifft.latency + fft.latency

  dataIn.payloadMap(ifftPre) >> ifft.dataIn
  fft.dataOut.payloadMap(fftPost) >> dataOut
}

class Rx0to1(actual: Int) extends RxLoop(Seq(0, 1), actual)
  with DSPTestable[Vec[ComplexNumber], Bits] {

  override val dataIn = slave Stream Vec(symbolComplexType, 256)
  override val dataOut = master Stream Bits(512 bits)
  override val latency = qamdemod.latency + deInterleave.latency + vitdecs.latency

  dataIn >> qamdemod.dataIn
  vitdecs.dataOut >> dataOut
}

class Rx0to2(actual: Int) extends RxLoop(Seq(0, 1, 2), actual)
  with DSPTestable[Vec[ComplexNumber], Bits] {

  override val dataIn = slave Stream Vec(symbolComplexType, 256)
  override val dataOut = master Stream Bits(1024 bits)
  override val latency = qamdemod.latency + deInterleave.latency + vitdecs.latency + convencs.latency

  dataIn >> qamdemod.dataIn
  convencs.dataOut >> dataOut
}

class Rx3to4() extends RxLoop(Seq(3, 4))
  with DSPTestable[Bits, Vec[ComplexNumber]] {

  override val dataIn = slave Stream Bits(1024 bits)
  override val dataOut = master Stream Vec(symbolComplexType, 256)
  override val latency = interleave.latency + qammod.latency + ifft.latency + fft.latency

  dataIn.payloadMap(bits2bools) >> interleave.dataIn
  fft.dataOut.payloadMap(fftPost) >> dataOut
}

class RxLoopWhole(actual:Int) extends RxLoop(Seq(0, 1, 2, 3, 4), actual)
  with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  override val dataIn = slave Stream Vec(symbolComplexType, 256)
  override val dataOut = master Stream Vec(symbolComplexType, 256)
  override val latency =
    qamdemod.latency + deInterleave.latency + vitdecs.latency + convencs.latency + interleave.latency + qammod.latency + ifft.latency + fft.latency

  dataIn >> qamdemod.dataIn
  fft.dataOut.payloadMap(fftPost) >> dataOut
}