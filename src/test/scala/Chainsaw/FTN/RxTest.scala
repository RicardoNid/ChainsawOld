package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec

class RxTest extends AnyFlatSpec {

  val testSize = 8 // number of frames used for test
  val parallelismForTest = 128
  require(parallelismForTest % 4 == 0)

  val dataWithPreamble = {
    val frame = rxModulateGolden.flatten.grouped(128).toSeq
    (0 until testSize).flatMap(_ => frame)
  }

  val realSymbolLength = rxModulateGolden.flatten.length
  assert(realSymbolLength == 18 * 512)
  val frameLength = dataWithPreamble.length
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
  val iter6 = loadFTN1d[Double]("iter6").toSeq.grouped(512).toSeq // after ifft
  val iter7 = loadFTN1d[MComplex]("iter7").map(_.toBComplex).toSeq.grouped(256).toSeq // after fft

  Seq(iterIn, iter0, iter1, iter2, iter3, iter4, iter5, iter6, iter7).foreach(seq => assert(seq.length == 16))



  "RxFront" should "show the data" in {
    println(dataWithPreamble.head.mkString(" "))
    println(rxMappedGolden.head)
    println(rxEqualizedGolden.head)
  }

  "RxFront" should "work correctly" in {
    val goldens = (0 until testSize).flatMap(_ => rxEqualizedGolden)
    doFlowPeekPokeTest(
      dut = RxFront(), name = "testRxFront",
      testCases = dataWithPreamble, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }

  "Rx" should "work correctly on qamdemod and deInterleave" in {
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
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1,
      verbose = false
    )
  }

  it should "work correctly on ifft and fft" in {
    val data = (0 until testSize).flatMap(_ => iter5)
    val goldens = (0 until testSize).flatMap(_ => iter7)
    doFlowPeekPokeTest(
      dut = new Rx4, name = "testRx4",
      testCases = data, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1,
      verbose = false
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

  it should "work correctly on stage 0 to 1" in {
    val data = (0 until testSize).flatMap(_ => iterIn)
    val goldens = (0 until testSize).flatMap(_ => iter2)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = new Rx0to1(parallelismForTest), name = "testRx0to1",
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

  it should "work with iteration on last round" in {
    val data = dataWithPreamble
    val goldens = (0 until testSize).flatMap(_ => iter2)
//      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    println(data.length)
    doFlowPeekPokeTest(
      dut = Rx(parallelismForTest), name = "testRxWithIteration",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  it should "work with iteration for all" in {
    val data = dataWithPreamble
    val goldens = (0 until testSize).flatMap(_ => iter2)
      .zipWithIndex.map { case (big, i) => if (i < parallelismForTest / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = Rx(parallelismForTest), name = "testRxWithIteration",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }
}
