package Chainsaw.algos

import Chainsaw._
import Chainsaw.algos.AlgebraicStructures._
import breeze.linalg.DenseVector
import org.scalatest.flatspec.AnyFlatSpec

class NttTest extends AnyFlatSpec {

  val p = 3329
  implicit val zp = Zp(p)

  val numbers = DenseVector.tabulate(256)(_ => ChainsawRand.nextInt(p))

  behavior of "NTTTest"

  it should "ntt and intt" in Ntt.intt(Ntt.ntt(numbers)).equals(numbers)

}
