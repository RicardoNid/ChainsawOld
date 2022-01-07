package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class RxFront()
  extends Component with DSPTestable[Vec[SFix], Vec[ComplexNumber]] {

  val fft = DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4), Seq(4, 2), fftType, unitType)
  val s2p = DSP.S2P(128, 512, fftComplexType)
  val fifo = BigStreamFifo(equalizerComplexVecType, 18)
  val equalizer = EqualizerFTN(preambleSymbols)

  override val dataIn = slave(cloneOf(fft.dataIn))
  override val dataOut = master Stream Vec(unitComplexType, 256)
  override val latency = fft.latency + s2p.latency + equalizer.latency + 18 // 18 for fifo

  def equalizerPost(in: Vec[ComplexNumber]) = Vec(in.map(_.truncated(unitType)))

  def fftPost(in: Vec[ComplexNumber]) =
    Vec(in.take(in.length / 2).map(_ >> 9).map(_.truncated(equalizerType)))

  val speedCounter = CounterFreeRun(80) // control the overall throughput of RxFront

  dataIn >> fft.dataIn
  dataIn.allowOverride
  fft.dataIn.allowOverride
  dataIn.ready := speedCounter >= U(8)
  fft.dataIn.valid := (speedCounter >= U(8) && dataIn.valid)

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
