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

  "Rx synth" should "synth for the whole loop" in {
    VivadoSynthForTiming(new RxLoopWhole, "RxLoop")
  }

  it should "synth for RxFront" in VivadoSynthForTiming(RxFront())

  it should "synth for all the components in RxLoop" in {
    import channelInfo._
    VivadoSynthForTiming(comm.qam.AdaptiveQamdemod(bitAlloc, powAlloc, rxUnitComplexType), "qamdemodRx")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(64, 256, 1024, 1024, HardType(Bool())), "interleaveRx")
    VivadoSynthForTiming(Convenc512FTN(), "convencRx")
    VivadoSynthForTiming(comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType), "qammodRx")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(256, 64, 1024, 1024, HardType(Bool())), "interleaveRx")
    // most resource-consuming parts
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyHSIFFT(512, Seq(4, 4, 4, 4), Seq(2), ifftType, rxUnitType), "fftRx")
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4, 4), Seq(2), fftType, rxUnitType), "ifftRx")
  }

  it should "synth for all components in RxFront" in {
    VivadoSynthForTiming(EqualizerFTN(preambleSymbols), name = "equalizerRxFront")
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyRVFFT(512, Seq(4, 4, 4), Seq(4, 2), fftType, rxUnitType), name = "fftRxFront")
  }

  it should "synth for sub modules of vitdec" in {
    val parallelism = 512
    //    VivadoSynthForTiming(ParallelVitFTN(512, 1), "vitdecRxWrapper")
    VivadoSynthForTiming(ParallelVitFTN(512, 512), "vitdecRx")
    //    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(parallelism, 256, 2 * parallelism, 2 * parallelism, Bool), "interleaveForParallelVit")
    //    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(128, parallelism, parallelism, parallelism, Bool), "deInterleaveForParallelVit")
    //    VivadoSynthForTiming(comm.viterbi.ViterbiHardware(trellis = trellis, length = 128, copies = parallelism, readAsync = false, disWidth = 4), "parallelVit")
  }

}
