package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class RxTest extends AnyFlatSpec {

  val testSize = 8
  val data = (0 until testSize).flatMap(_ => modulateGolden)

  val dataWithPreamble = {
    val frame = (preambleModulatedGolden ++ modulateGolden).flatten.grouped(128).toSeq
    (0 until testSize).flatMap(_ => frame)
  }

  val realSymbolLength = (preambleModulatedGolden ++ modulateGolden).flatten.length
  assert(realSymbolLength == 18 * 512)
  val frameLength = dataWithPreamble.length
  assert(frameLength == 18 * 4 * testSize)

  def boolSeq2BigInt(in: Seq[Int]) = BigInt(in.mkString(""), 2)

  val iterIn: Seq[Seq[BComplex]] = loadFTN1d[MComplex]("iterIn").map(_.toBComplex).toSeq.grouped(256).toSeq // symbol
  val iter0: Seq[BigInt] = loadFTN1d[Double]("iter0").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after qamdemod
  val iter1: Seq[BigInt] = loadFTN1d[Double]("iter1").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after deInterleave
  val iter1ByGroup: Seq[BigInt] = loadFTN1d[Double]("iter1").map(_.toInt).toSeq.grouped(256).toSeq.map(boolSeq2BigInt) // after deInterleave
  val iter2: Seq[BigInt] = loadFTN1d[Double]("iter2").map(_.toInt).toSeq.grouped(512).toSeq.map(boolSeq2BigInt) // after vit
  val iter2ByGroup: Seq[BigInt] = loadFTN1d[Double]("iter2").map(_.toInt).toSeq.grouped(128).toSeq.map(boolSeq2BigInt) // after vit
  val iter3: Seq[BigInt] = loadFTN1d[Double]("iter3").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after conv
  val iter4: Seq[BigInt] = loadFTN1d[Double]("iter4").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after interleave
  val iter5 = loadFTN1d[MComplex]("iter5").map(_.toBComplex).toSeq.grouped(256).toSeq // after qammod
  val iter6 = loadFTN1d[Double]("iter6").toSeq.grouped(512).toSeq // after ifft
  val iter7 = loadFTN1d[MComplex]("iter7").map(_.toBComplex).toSeq.grouped(256).toSeq // after fft

  Seq(iterIn, iter0, iter1, iter2, iter3, iter4, iter5, iter6, iter7).foreach(seq => assert(seq.length == 16))

  val parallelismForTest = 4
  require(parallelismForTest % 4 == 0)

  "RxFront" should "work correctly" in {
    val goldens = (0 until testSize).flatMap(_ => equalizedGolden)
    doFlowPeekPokeTest(
      dut = RxFront(), name = "testRxFront",
      testCases = dataWithPreamble, golden = goldens,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  it should "synth" in VivadoSynthForTiming(RxFront())

  "Rx" should "work correctly on qamdemod and deInterleave" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter1)
    doFlowPeekPokeTest(
      dut = new Rx0, name = "testRx0",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on smaller vitdec" in {
    val parallelism = 512

    val data: Seq[BigInt] = (0 until testSize).flatMap(_ => iter1)
    val goldens = (0 until testSize).flatMap(_ => iter2)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = ParallelVitFTN(parallelism, parallelismForTest), name = "testVit",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  "Rx" should "work correctly on vitdec" in {
    val data = (0 until testSize).flatMap(_ => iter1)
    val goldens = (0 until testSize).flatMap(_ => iter2)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx1(parallelismForTest), name = "testRx1",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  "Rx" should "work correctly on convenc" in {
    val data = (0 until testSize).flatMap(_ => iter2)
    val goldens = (0 until testSize).flatMap(_ => iter3)
    doFlowPeekPokeTest(
      dut = new Rx2, name = "testRx2",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on interleave and qammod" in {
    val data = (0 until testSize).flatMap(_ => iter3)
    val goldens = (0 until testSize).flatMap(_ => iter5)
    doFlowPeekPokeTest(
      dut = new Rx3, name = "testRx2",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  it should "work correctly on ifft and fft" in {
    val data = (0 until testSize).flatMap(_ => iter5)
    val goldens = (0 until testSize).flatMap(_ => iter7)
    doFlowPeekPokeTest(
      dut = new Rx4, name = "testRx3",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  it should "work correctly on stage 0 to 2" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter3)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx0to2(parallelismForTest), name = "testRx0to2",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on stage 3 to 4" in {
    val data = (0 until testSize).flatMap(_ => iter3)
    val goldens = (0 until testSize).flatMap(_ => iter7)
    doFlowPeekPokeTest(
      dut = new Rx3to4, name = "testRx3to4",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  it should "synth for the whole loop" in {
    VivadoSynthForTiming(new RxWhole, "RxLoop")
  }

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

  it should "synth for sub modules of important components" in {
    val parallelism = 512
    VivadoSynthForTiming(ParallelVitFTN(512, 512), "vitdecRx")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(parallelism, 256, 2 * parallelism, 2 * parallelism, Bool), "interleaveForParallelVit")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(128, parallelism, parallelism, parallelism, Bool), "deInterleaveForParallelVit")
    VivadoSynthForTiming(comm.viterbi.ViterbiHardware(trellis = trellis, length = 128, copies = parallelism, readAsync = false, disWidth = 5), "parallelVit")
  }
}
