package Chainsaw.algos

import Chainsaw._
import Chainsaw.algos.TerminationMode._
import breeze.linalg._
import org.scalatest.flatspec.AnyFlatSpec

class ConvencTest extends AnyFlatSpec {

  behavior of "ConvencTest"

  val testSize = 10
  val testLength = 128

  val matlabTrellis = MatlabRefs.poly2trellis(7, Array(171, 133))
  val trellis = Trellis.fromMatlab(matlabTrellis)

  // preparing data
  val originals = (0 until testSize)
    .map(_ => ChainsawRand.nextBits(testLength - 6).padTo(testLength, 0))
    .map(data => new DenseVector(data.toArray))

  val goldens: Seq[DenseVector[Int]] = originals.map(MatlabRefs.convenc(_, matlabTrellis))

  type convencAlgo = (DenseVector[Int], Trellis[Int]) => DenseVector[Int]

  @matlab
  def testConvencAlgo(algo: convencAlgo): Unit = {
    val yours: Seq[DenseVector[Int]] = originals.map(algo(_, trellis)).map(symbols2Bits(_, 2))
    yours.indices.foreach { i =>
      yours.zip(goldens).foreach { case (a, b) =>
        assert(a == b, s"\nyours:  ${a.toArray.mkString("")} ${a.length}\ngolden: ${b.toArray.mkString("")} ${b.length}")
      }
    }
  }

  "convenc algorithm" should "work" in testConvencAlgo(Convenc.convenc)

}
