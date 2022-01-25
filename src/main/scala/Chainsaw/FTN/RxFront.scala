package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class RxFront(implicit ftnParams: FtnParams)
  extends Component with DSPTestable[Vec[SInt], Vec[ComplexNumber]] {

  override val dataIn = slave Stream Vec(ADDAType, 128)
  override val dataOut = master Stream Vec(symbolComplexType, 256)

  // components
  val fft = DSP.FFT.CooleyTukeyRVFFT(512, 128, addaFftType, coeffType, fftDecomposition, frontFftShifts)
  val s2p = DSP.S2P(128, 512, toComplexType(fft.retDataType))
  val fifo = BigStreamFifo(smootherComplexVecType, 18)
  val equalizer = EqualizerFTN(preambleSymbols)

  override val latency = fft.latency + s2p.latency + equalizer.latency + 18 // 18 for fifo

  def sint2SFix(in: Vec[SInt]) = {
    val ret = cloneOf(fft.dataIn.payload)
    ret.zip(in).foreach { case (fix, int) => fix assignFromBits int ## B"0000000000" }
    ret
  }

  val speedCounter = CounterFreeRun(80) // control the overall throughput of RxFront by a "window"
  val open = speedCounter >= U(8)

  // connections
  // dataIn -> fft
  dataIn
    .payloadMap(sint2SFix)
    .withEnable(open) >> fft.dataIn

  fft.dataOut >> s2p.dataIn

  s2p.dataOut
    .payloadMap(fftPost)
    .payloadMap(shiftRight(_, 9))
    .payloadMap(truncComplex(_, smootherType)) >> fifo.io.push

  // fifo -> equalizer burst transfer
  val burst = Trigger(fifo.io.occupancy === U(18), 18)
  fifo.io.pop
    .withEnable(burst) >> equalizer.dataIn

  // equalizer -> dataOut
  equalizer.dataOut
    .payloadMap(truncComplex(_, symbolType)) >> dataOut
}