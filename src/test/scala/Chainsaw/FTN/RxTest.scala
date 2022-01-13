package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec

class RxTest extends AnyFlatSpec {

  // for vit
  val testSize = 1 // number of frames used for test
  val parallelismForTest = 64

  // for whole
  //  val testSize = 128 // number of frames used for test
  //  val parallelismForTest = 512

  require(parallelismForTest % 4 == 0)

  RunFTN()

  // data for RxFront
  val rxModulated = loadFTN1d[Double]("rxScaled")
  val rxMapped = loadFTN1d[MComplex]("rxMapped").map(_.toBComplex)
  val rxEqualized = loadFTN1d[MComplex]("rxEqualized").map(_.toBComplex)

  val rxModulateGolden: Seq[Seq[Double]] = {
    val (preamble, data) = rxModulated.splitAt(1024)
    preamble.grouped(512).map(_.toSeq).toSeq ++ data.grouped(450).map(_.toSeq.padTo(512, 0.0)).toSeq
  }
  val rxMappedGolden: Seq[Seq[BComplex]] = rxMapped.grouped(256).map(_.toSeq).toSeq
  val rxEqualizedGolden: Seq[Seq[BComplex]] = rxEqualized.grouped(256).map(_.toSeq).toSeq

  val dataWithPreamble: Seq[Seq[BigInt]] = {
    val frame = rxModulateGolden.flatten.grouped(128).toSeq
    (0 until testSize).flatMap(_ => frame)
  }.map(_.map(value => BigInt(value.toInt)))

  val realSymbolLength = rxModulateGolden.flatten.length
  assert(realSymbolLength == 18 * 512)
  val frameLength = dataWithPreamble.length
  assert(frameLength == 18 * 4 * testSize)
  assert(frameLength == 18 * 4 * testSize)

  def boolSeq2BigInt(in: Seq[Int]) = BigInt(in.mkString(""), 2)

  // loading data for test
  val iterIn: Seq[Seq[BComplex]] = loadFTN1d[MComplex]("iterIn").map(_.toBComplex).toSeq.grouped(256).toSeq // symbol
  val iter0: Seq[BigInt] = loadFTN1d[Double]("iter0").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after qamdemod
  val iter1: Seq[BigInt] = loadFTN1d[Double]("iter1").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after deInterleave
  val iter1ByGroup: Seq[BigInt] = loadFTN1d[Double]("iter1").map(_.toInt).toSeq.grouped(256).toSeq.map(boolSeq2BigInt) // after deInterleave
  val iter2: Seq[BigInt] = loadFTN1d[Double]("iter2").map(_.toInt).toSeq.grouped(512).toSeq.map(boolSeq2BigInt) // after vit
  val iter2ByGroup: Seq[BigInt] = loadFTN1d[Double]("iter2").map(_.toInt).toSeq.grouped(128).toSeq.map(boolSeq2BigInt) // after vit
  val iter3: Seq[BigInt] = loadFTN1d[Double]("iter3").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after conv
  val iter4: Seq[BigInt] = loadFTN1d[Double]("iter4").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after interleave
  val iter5 = loadFTN1d[MComplex]("iter5").map(_.toBComplex).toSeq.grouped(256).toSeq // after qammod
  val iter6 = loadFTN1d[Double]("iter6").toSeq.grouped(512).toSeq.map(_.map(_ * 512.0)) // after ifft
  val iter7 = loadFTN1d[MComplex]("iter7").map(_.toBComplex).toSeq.grouped(256).toSeq // after fft
  val diff = loadFTN1d[MComplex]("diff").map(_.toBComplex).toSeq.grouped(256).toSeq // after fft

  val finalDeInterleaved = loadFTN1d[Double]("rxFinalDeInterleaved").map(_.toInt).toSeq.grouped(1024).toSeq.map(boolSeq2BigInt) // after final vitdec
  val finalDecoded: Seq[BigInt] = loadFTN1d[Double]("rxFinalDecoded").map(_.toInt).toSeq.grouped(512).toSeq.map(boolSeq2BigInt) // after final vitdec

  Seq(iterIn, iter0, iter1, iter2, iter3, iter4, iter5, iter6, iter7).foreach(seq => assert(seq.length == 16))

  val biterr = loadFTN1d[Double]("rxFinalDecoded").map(_.toInt)
    .zip(loadFTN1d[Double]("txRaw").map(_.toInt))
    .filter { case (rx, tx) => rx != tx }.length / 8192.0

  logger.info(s"bit err of matlab is $biterr")

  "Data" should "be loaded" in {
    logger.info(s"max & min: ${dataWithPreamble.flatten.max}, ${dataWithPreamble.flatten.min}")
    logger.info(s"data before ifft, max: ${iter5.flatten.map(_.real).max max iter5.flatten.map(_.imag).max}")
    logger.info(s"data before fft, max: ${iter6.flatten.max}")
    logger.info(s"data after fft, max: ${iter7.flatten.map(_.real).max max iter7.flatten.map(_.imag).max}")

    //    println(dataWithPreamble.map(_.map(_.toString(16)).mkString(" ")).mkString("\n"))
    printlnGreen("equalized")
    println(rxEqualizedGolden.map(_.mkString(" ")).mkString("\n"))
    //    println(iter0.map(_.toString(2).padToLeft(1024, '0')
    //      .grouped(4).toSeq.map(chars => BigInt(chars, 2).toString(16)).mkString("")).mkString("\n"))
    //    println(iter1.map(_.toString(16)).mkString("\n"))
    //    println(iter2.mkString("\n"))
    printlnGreen("after qammod")
    println(iter5.map(_.mkString(" ")).mkString("\n"))
    printlnGreen("after fft")
    println(iter7.map(_.mkString(" ")).mkString("\n"))
    printlnGreen("diff")
    println(diff.map(_.mkString(" ")).mkString("\n"))
  }

//  "RxFront" should "work correctly on fft" in {
//    val goldens = (0 until testSize).flatMap(_ => rxMappedGolden)
//    doFlowPeekPokeTest(
//      dut = RxFrontFft(), name = "testRxFrontFft",
//      testCases = dataWithPreamble, golden = goldens,
//      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-3
//    )
//  }

  "EqualizerFTN" should "work correctly on equalization" in {
    doFlowPeekPokeTest(
      dut = EqualizerFTN(preambleSymbols), name = "testEqualizerFTN",
      testCases = rxMappedGolden, golden = rxEqualizedGolden,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  "RxFront" should "work correctly as a whole" in {
    val goldens = (0 until testSize).flatMap(_ => rxEqualizedGolden)
    doFlowPeekPokeTest(
      dut = RxFront(), name = "testRxFront",
      testCases = dataWithPreamble, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-2
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
    val data = (0 until testSize).flatMap(_ => loadFTN1d[MComplex]("txMapped").map(_.toBComplex).grouped(256).toSeq.map(_.toSeq))
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

  "RxFull" should "work correctly until vitdec" in {
    val data = dataWithPreamble
    val goldens = (0 until testSize).flatMap(_ => iter1)
    doFlowPeekPokeTest(
      dut = RxFrontMore(), name = "testRxWithFde",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly until the end of the first iteration" in {
    val data = dataWithPreamble
    val goldens = (0 until testSize).flatMap(_ => iter7)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = RxFullUntilQamdemod(parallelismForTest), name = "testRxLoopWithFde",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-2
    )
  }

  it should "gen for full Rx" in GenRTL(RxFull(512))

  it should "work with all iterations" in {
    val data = dataWithPreamble
    val goldens = (0 until testSize).flatMap(_ => finalDecoded)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }

//    doFlowPeekPokeTest(
//      dut = RxFull(parallelismForTest, 2), name = "testRxFullIter_1",
//      testCases = data, golden = goldens,
//      testMetric = TestMetric.SAME
//    )

    doFlowPeekPokeTest(
      dut = RxFull(512), name = "testRxFullIter_4",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }
}
