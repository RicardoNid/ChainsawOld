package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class RxFront()
  extends Component with DSPTestable[Vec[SInt], Vec[ComplexNumber]] {

  val fftInType = HardType(SFix(5 exp, -6 exp))
  val frontFftShifts = Seq(2, 2, 1, 0, 0) // this part has a wider dynamic range
  // components
  val fft = DSP.FFT.CooleyTukeyRVFFT(512, 128, fftInType, coeffType, Seq(4, 4, 4, 4, 2), frontFftShifts)
  val s2p = DSP.S2P(128, 512, toComplexType(fft.retDataType))
  val fifo = BigStreamFifo(equalizerComplexVecType, 18)
  val equalizer = EqualizerFTN(preambleSymbols)

  override val dataIn = slave Stream Vec(ADDAType, 256)
  override val dataOut = master Stream Vec(symbolComplexType, 256)
  override val latency = fft.latency + s2p.latency + equalizer.latency + 18 // 18 for fifo

  def doScaling(in:Vec[SInt]) = {
    val ret = cloneOf(fft.dataIn.payload)
    ret.zip(in).foreach{ case (fix, int) => fix assignFromBits int ## B"000000"}
    ret
  }

  def fftPost(in: Vec[ComplexNumber]) = // to the range for channel equalization
    Vec(in.take(in.length / 2).map(_ >> 8).map(_.truncated(equalizerType)))

  def equalizerPost(in: Vec[ComplexNumber]) = Vec(in.map(_.truncated(symbolType)))

  val speedCounter = CounterFreeRun(80) // control the overall throughput of RxFront

  dataIn.t(doScaling) >> fft.dataIn
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
