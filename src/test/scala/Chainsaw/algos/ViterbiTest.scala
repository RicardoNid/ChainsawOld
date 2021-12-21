package Chainsaw.algos

import Chainsaw._
import Chainsaw.algos.TerminationMode._
import breeze.linalg._
import org.scalatest.flatspec.AnyFlatSpec

class ViterbiTest extends AnyFlatSpec {

  behavior of "ViterbiTest"

  // defining model
  val testSize = 10
  val testLength = 128

  val testTrellis = MatlabRefs.poly2trellis(7, Array(171, 133))
  val goldenModel: (DenseVector[Int], TerminationMode) => DenseVector[Int] = MatlabRefs.vitdecHard(_, testTrellis, 42, _)

  // preparing data
  val originals = (0 until testSize)
    .map(_ => ChainsawRand.nextBits(testLength - 6).padTo(testLength, 0))
    .map(data => new DenseVector(data.toArray))

  val coded: Seq[DenseVector[Int]] = originals.map(MatlabRefs.convenc(_, testTrellis))
  val noise = new DenseVector((0 until testLength * 2).map(i => if (i % 20 == 0) 1 else 0).toArray)
  val polluted = coded.map(data => (data + noise) % 2)

  val goldens0 = coded.map(data => goldenModel(data, TerminationMode.TERMINATION))
  val goldens1 = polluted.map(data => goldenModel(data, TerminationMode.TERMINATION))

  // test method
  type viterbiHardAlgo = (DenseVector[Int], Trellis[Int], (Int, Int) => Double, TerminationMode) => DenseVector[Int]

  @matlab
  def testViterbiAlgo(algo: viterbiHardAlgo) = {
    val yours = coded.map(data => algo(Viterbi.bits2Symbols(data, 2), Trellis.fromMatlab(testTrellis), Metrics.Hamming, TERMINATION))
    yours.zip(goldens0).foreach{ case (a, b) => assert(a == b, s"yours:  $a\ngolden: $b")}
    yours.zip(goldens1).foreach{ case (a, b) => assert(a == b, s"yours:  $a\ngolden: $b")}
  }

  // test tasks
  it should "viterbi" in testViterbiAlgo(Viterbi.viterbi[Int])

}
