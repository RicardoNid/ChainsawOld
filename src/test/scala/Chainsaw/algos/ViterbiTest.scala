package Chainsaw.algos

import Chainsaw._
import Chainsaw.algos.TerminationMode._
import Chainsaw.comm.viterbi.ViterbiHardware
import Chainsaw.dspTest._
import breeze.linalg._
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
    val pollutedSymbols = polluted.map(Viterbi.bits2Symbols(_, 2))
    val symbols = Seq(codedSymbols, pollutedSymbols)
    val yours = Seq.tabulate(2, 2)((i, j) => symbols(i).map(algo(_, trellis, Metrics.Hamming, modes(j)))).flatten

    yours.indices.foreach { i =>
      yours(i).zip(goldens(i)).foreach { case (a, b) =>
        assert(a == b, s"\nyours:  ${a.toArray.mkString("")} ${a.length}\ngolden: ${b.toArray.mkString("")} ${b.length}")
      }
    }
  }

  // test tasks
  it should "viterbi" in testViterbiAlgo(Viterbi.viterbi[Int])

  it should "viterbiTraceback" in testViterbiAlgo(Viterbi.viterbiTraceback[Int])

  it should "work on hardware by termination mode" in {

    GenRTL(ViterbiHardware(trellis, testLength))

    // run algo first, so you can generate info to compare with
    val symbols = (coded ++ polluted).map(Viterbi.bits2Symbols(_, 2))
    val golden = goldens(0) ++ goldens(2)
    val ret = symbols.map(Viterbi.viterbiTraceback(_, trellis, Metrics.Hamming, TERMINATION))
    assert(ret.zip(golden).forall { case (a, b) => a == b })

    doFlowPeekPokeTest(
      name = "testViterbiHardware",
      dut = ViterbiHardware(trellis, testLength, readAsync = false),
      testCases = symbols.flatMap(_.toArray.map(BigInt(_))),
      golden = golden.flatMap(_.toArray.map(BigInt(_)))
    )
  }

  it should "synth" in {
//    VivadoSynth(ViterbiHardware(trellis, 128), s"VitdecForFtnUsingLUT")
    VivadoSynth(ViterbiHardware(trellis, 128, readAsync = false, disWidth = 4), s"VitdecForFtnUsingBRAM")
  }

}
