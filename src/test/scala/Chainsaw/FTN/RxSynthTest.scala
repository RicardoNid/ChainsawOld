package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class RxSynthTest extends AnyFlatSpec {

  "Rx Gen" should "show the overall latency" in {
    GenRTL(new RxLoopWhole)
  }

  "Rx synth" should "synth for the whole loop" in VivadoSynthForTiming(new RxLoopWhole, "RxLoop")

  it should "synth for RxFront" in VivadoSynthForTiming(RxFront(), "RxFront")

  it should "synth for small components in RxLoop" in {
    import channelInfo._
//    VivadoSynthForTiming(comm.qam.AdaptiveQamdemod(bitAlloc, powAlloc, rxUnitComplexType), "qamdemodRx")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(64, 256, 1024, 1024, HardType(Bool())), "interleaveRx")
//    VivadoSynthForTiming(Convenc512FTN(), "convencRx")
//    VivadoSynthForTiming(comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType), "qammodRx")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(256, 64, 1024, 1024, HardType(Bool())), "deInterleaveRx")
  }

  it should "synth for all components in RxFront" in {
    VivadoSynthForTiming(EqualizerFTN(preambleSymbols), name = "equalizerRxFront")
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4), Seq(4, 2), fftType, unitType), name = "fftRxFront")
  }

  it should "synth for sub modules of vitdec" in {
    VivadoSynthForTiming(ParallelVitFTN(512, 512), "vitdecRx")
    val parallelism = 512
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(parallelism, 256, 2 * parallelism, 2 * parallelism, Bool), "interleaveForParallelVit")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(128, parallelism, parallelism, parallelism, Bool), "deInterleaveForParallelVit")
    VivadoSynthForTiming(comm.viterbi.ViterbiHardware(trellis = trellis, length = 128, copies = parallelism, readAsync = false, disWidth = 4), "parallelVit")
  }

  it should "synth for sub modules of ifft/fft" in {
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyHSIFFT(512, Seq(4, 4, 4, 4), Seq(2), ifftType, unitType), "fftRx")
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4, 4), Seq(2), fftType, unitType), "ifftRx")

    import DSP.FFT._
    val N = 512
    val factors1 = Seq(4, 4, 4, 4)
    val factors2 = Seq(2)
    val pF = factors1.product * 2

    VivadoSynthForTiming(HSPreprocess(N, ifftType), "ifftRxPre")
    VivadoSynthForTiming(CooleyTukeyBackToBack(N, pF / 2, factors1, factors2, true, ifftType, unitType), "ifftRxCore")
    VivadoSynthForTiming(HSPostprocess(N, ifftType), "ifftRxPost")

    VivadoSynthForTiming(RVPreprocess(N, fftType), "fftRxPre")
    VivadoSynthForTiming(CooleyTukeyBackToBack(N, pF / 2, factors1, factors2, false, fftType, unitType), "fftRxCore")
    VivadoSynthForTiming(RVPostprocess(N, fftType), "fftRxPost")
  }

  "RxFull" should "gen successfully" in GenRTL(RxFull(512), name = "RxFull")

  "RxFull" should "synth successfully" in VivadoSynth(RxFull(512), "RxFull")

  it should "synth for its memories" in {
    val fdeType = HardType(Vec(unitComplexType, 256))
    val loopLength = 608
    val frameLength = 16
    val iteration = 5
//    VivadoSynth(BigStreamFifo(fdeType, 2 * frameLength))
    VivadoSynth(LoopBuffer(fdeType, loopLength, frameLength, iteration))
  }

}
