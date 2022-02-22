package Chainsaw.algos

import Chainsaw._
import Chainsaw.algos.TerminationMode._
import Chainsaw.comm.viterbi.ViterbiHardware
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import breeze.linalg.{DenseMatrix, DenseVector, princomp}
import org.scalatest.flatspec.AnyFlatSpec

class ViterbiTest extends AnyFlatSpec {

  behavior of "ViterbiTest"

  // defining model
  val testSize = 10
  val testLength = 128

  val matlabTrellis = MatlabRefs.poly2trellis(7, Array(171, 133))
  val trellis = Trellis.fromMatlab(matlabTrellis)
  val goldenModel: (DenseVector[Int], TerminationMode) => DenseVector[Int] = MatlabRefs.vitdecHard(_, matlabTrellis, 8, _)

  // preparing data
  val originals = (0 until testSize)
    .map(_ => ChainsawRand.nextBits(testLength - 6).padTo(testLength, 0))
    .map(data => new DenseVector(data.toArray))

  val coded: Seq[DenseVector[Int]] = originals.map(MatlabRefs.convenc(_, matlabTrellis))
  val noise = new DenseVector((0 until testLength * 2).map(i => if (i % 20 == 0) 1 else 0).toArray)
  val polluted = coded.map(data => (data + noise) % 2)

  val testCases = Seq(coded, polluted)
  val modes = Seq(TERMINATION, TRUNCATION)
  val goldens = Seq.tabulate(testCases.length, modes.length)((i, j) =>
    testCases(i).map(data => goldenModel(data, modes(j)))).flatten

  // test method
  type viterbiHardAlgo = (DenseVector[Int], Trellis[Int], (Int, Int) => Double, TerminationMode) => DenseVector[Int]

  @matlab
  def testViterbiAlgo(algo: viterbiHardAlgo): Unit = {
    val codedSymbols: Seq[DenseVector[Int]] = coded.map(Viterbi.bits2Symbols(_, 2))
    val pollutedSymbols: Seq[DenseVector[Int]] = polluted.map(Viterbi.bits2Symbols(_, 2))

    val symbols = Seq(codedSymbols, pollutedSymbols)
    val yours = Seq.tabulate(2, 2)((i, j) => symbols(i).map(algo(_, trellis, Metrics.Hamming, modes(j)))).flatten

    yours.indices.foreach { i =>
      yours(i).zip(goldens(i)).foreach { case (a, b) =>
        assert(a == b, s"\nyours:  ${a.toArray.mkString("")} ${a.length}\ngolden: ${b.toArray.mkString("")} ${b.length}")
      }
    }
  }

  // test tasks
  "viterbi algorithm" should "work" in testViterbiAlgo(Viterbi.viterbi[Int])

  "viterbiTraceback algorithm" should "work" in testViterbiAlgo(Viterbi.viterbiTraceback[Int])

  it should "work for FTN" in {
    val pollutedSymbols: Seq[DenseVector[Int]] = FTN.loadFTN1d[Double]("iter1")
      .grouped(256).toSeq
      .map(doubles => new DenseVector(doubles.map(_.toInt)))
      .map(Viterbi.bits2Symbols(_, 2))
    val decodedSymbols = FTN.loadFTN1d[Double]("iter2")
      .grouped(128).toSeq
      .map(doubles => new DenseVector(doubles.map(_.toInt)))
      .map(Viterbi.bits2Symbols(_, 1))

    var diff = 0
    pollutedSymbols.zip(decodedSymbols).zipWithIndex.foreach { case ((polluted, decoded), i) =>
      //      println(s"result: $i / ${pollutedSymbols.length}")
      val yours = Viterbi.viterbiTraceback(polluted, trellis, Metrics.Hamming, TerminationMode.TERMINATION)
      if (yours != decoded) {
        diff += 1
        printlnRed(s"coded:  ${polluted.toArray.mkString("   ")}")
        printlnRed(s"matlab: ${decoded.toArray.mkString("   ")}")
        printlnRed(s"yours:  ${yours.toArray.mkString("   ")}")
        printlnRed(s"diff:   ${decoded.toArray.zip(yours.toArray).map { case (i, i1) => if (i == i1) ' ' else 'x' }.mkString("   ")}")
      }
    }
    println(s"diff count: $diff")
  }

  "viterbi hardware" should "work on hardware by termination mode" in {

    GenRTL(ViterbiHardware(trellis, testLength))

    // run algo first, so you can generate info to compare with
    val symbols: Seq[DenseVector[Int]] = (coded ++ polluted).map(Viterbi.bits2Symbols(_, 2))
    val golden: Seq[DenseVector[Int]] = goldens(0) ++ goldens(2)
    val ret = symbols.map(Viterbi.viterbiTraceback(_, trellis, Metrics.Hamming, TERMINATION))
    assert(ret.zip(golden).forall { case (a, b) => a == b })

    doFlowPeekPokeTest(
      name = "testViterbiHardware",
      dut = ViterbiHardware(trellis, testLength, readAsync = false, disWidth = 8),
      testCases = symbols.flatMap(_.toArray.map(BigInt(_))).map(data => Seq.fill(1)(data)),
      golden = golden.flatMap(_.toArray.map(BigInt(_))).map(data => Seq.fill(1)(data))
    )
  }

  it should "work for FTN" in {
    val testCases: Array[Double] = FTN.loadFTN1d[Double]("iter1")
    val goldens: Array[Double] = FTN.loadFTN1d[Double]("iter2")

    val parallelism = 1
    val testCase: Array[Seq[BigInt]] = Viterbi.bits2Symbols(new DenseVector(testCases.map(_.toInt)), 2)
      .toArray.map(BigInt(_)).map(data => Seq.fill(parallelism)(data))
    val golden: Array[Seq[BigInt]] = goldens.map(_.toInt).map(BigInt(_))
      .map(data => Seq.fill(parallelism)(data))

    doFlowPeekPokeTest(
      name = "testViterbiHardware",
      dut = ViterbiHardware(trellis, testLength, parallelism, readAsync = false, disWidth = 8),
      testCases = testCase,
      golden = golden
    )
  }

  it should "synth for parallel situation" in VivadoSynthForTiming(
    ViterbiHardware(trellis, 128, 16, readAsync = false, disWidth = 4),
    s"Vitdec512ForFtnUsingBRAM")


  import Chainsaw.DSP.FFT.CooleyTukeyFFT
//  CooleyTukeyFFT()










}
