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

// test of RxFront and RxLoop
class RxTest extends AnyFlatSpec {

  implicit val ftnParams: FtnParams = FtnParams(2, 256, doBitAlloc = false)

  import ftnParams._

  // for vit
  val testSize = 1 // number of frames used for test
  val parallelismForTest = 64

  def getPartForTest(in: Seq[BigInt], parallelismForTest: Int) =
    in.zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }

  val biterr = loadFTN1d[Double]("rxFinalDecoded").map(_.toInt)
    .zip(loadFTN1d[Double]("txRaw").map(_.toInt)).count { case (rx, tx) => rx != tx } / 8192.0

  logger.info(s"bit err of matlab is $biterr")

  val syncData: Seq[Seq[BigInt]] = eng.load[Array[Int]]("syncData").map(BigInt(_))
    .grouped(128).toSeq.map(_.toSeq)

  "RxFrontFFT" should "work correctly" in {
    doFlowPeekPokeTest(
      dut = new Component with DSPTestable[Vec[SInt], Vec[ComplexNumber]] {
        val fft = DSP.FFT.CooleyTukeyRVFFT(512, 128, addaFftType, coeffType, fftDecomposition, Seq(2, 2, 2, 0, 0))
        val s2p = DSP.S2P(128, 512, toComplexType(fft.retDataType))
        override val dataIn = slave Stream Vec(ADDAType, 128)
        override val dataOut = master Stream Vec(smootherComplexType, 256)
        override val latency = fft.latency + s2p.latency

        def sint2SFix(in: Vec[SInt]) = {
          val ret = cloneOf(fft.dataIn.payload)
          ret.zip(in).foreach { case (fix, int) => fix assignFromBits int ## B"0000000000" }
          ret
        }

        dataIn.payloadMap(sint2SFix) >> fft.dataIn
        fft.dataOut >> s2p.dataIn
        s2p.dataOut
          .payloadMap(fftPost)
          .payloadMap(shiftRight(_, 9))
          .payloadMap(truncComplex(_, smootherType)) >> dataOut

      }, name = "testFrontFFT",
      testCases = syncData,
      golden = rxMappedGolden,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  "EqualizerFTN" should "work correctly on equalization" in {
    doFlowPeekPokeTest(
      dut = EqualizerFTN(preambleSymbols), name = "testEqualizerFTN",
      testCases = rxMappedGolden, golden = rxEqualizedGolden,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  "RxFront" should "work correctly as a whole" in {
    //    val data = (0 until testSize).flatMap(_ => rxModulatedGolden)
    val data = (0 until testSize).flatMap(_ => syncData)
    println(syncData.map(_.mkString(" ")).mkString("\n"))
    val goldens = (0 until testSize).flatMap(_ => rxEqualizedGolden)
    doFlowPeekPokeTest(
      dut = RxFront(), name = "testRxFront",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  "RxLoop" should "work correctly on qamdemod and deInterleave" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter1)
    doFlowPeekPokeTest(
      dut = new Rx0, name = "testRx0",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on vitdec" in {
    val data = (0 until testSize).flatMap(_ => iter1)
    val goldens = (0 until testSize).flatMap(_ => iter2)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx1(parallelismForTest), name = "testRx1",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on vitdec in the final part" in {
    val data = (0 until testSize).flatMap(_ => finalDeInterleaved)
    val goldens = (0 until testSize).flatMap(_ => finalDecoded)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx1(parallelismForTest), name = "testFinalVit",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on convenc" in {
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
      dut = new Rx3, name = "testRx3",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-4)
  }

  it should "work correctly on ifft" in {
    val data = (0 until testSize).flatMap(_ => iter5)
    val goldens = (0 until testSize).flatMap(_ => iter6)
    doFlowPeekPokeTest(
      dut = new Rx4_0, name = "testRx4_0",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-3 * 512.0
    )
  }

  // to use this test, comment the line inside(4)(ifft.dataOut.payloadMap(ifftPost) >> fft.dataIn) in RxLoop
  it should "work correctly on fft" in {
    val data = (0 until testSize).flatMap(_ => iter6)
    val goldens = (0 until testSize).flatMap(_ => iter7)
    doFlowPeekPokeTest(
      dut = new Rx4_1, name = "testRx4_1",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 5E-3
    )
  }

  it should "work correctly on ifft and fft" in {
    val data = (0 until testSize).flatMap(_ => iter5)
    val goldens = (0 until testSize).flatMap(_ => iter7)
    doFlowPeekPokeTest(
      dut = new Rx4, name = "testRx4",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-2
    )
  }

  it should "work correctly on stage 0 to 1" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter2)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx0to1(parallelismForTest), name = "testRxLoopUntilConvenc",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "be able to decode good symbols" in {
    val data = (0 until testSize).flatMap(_ => txMapped)
    val goldens = (0 until testSize).flatMap(_ => finalDecoded)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx0to1(parallelismForTest), name = "testRxLoopUntilConvenc",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on stage 0 to 2" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter3)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx0to2(parallelismForTest), name = "testRxLoopUntilInterleave",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on stage 0 to 3" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter5)
    doFlowPeekPokeTest(
      dut = new Rx0to3(parallelismForTest), name = "testRxLoopUntilIfft",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-2
    )
  }

  it should "work correctly on as a whole" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter7)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new RxLoopWhole(parallelismForTest), name = "testRxLoopWhole",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }
}
