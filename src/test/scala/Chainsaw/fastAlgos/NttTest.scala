package Chainsaw.fastAlgos

import Chainsaw._
import breeze.linalg.DenseVector
import cc.redberry.rings.scaladsl.{Zp64, Ring}
import org.scalatest.flatspec.AnyFlatSpec
import Ntt._
import Convolution.cyclicConvolve

import scala.reflect.ClassTag

class NttTest extends AnyFlatSpec {

  val p = 3329
  implicit val ring:Ring[Long] = Zp64(p)
  val numbers = DenseVector.tabulate(256)(_ => ChainsawRand.nextInt(p))
  println(ntt(numbers))

  behavior of "NTTTest"

  it should "ntt and intt" in Ntt.intt(Ntt.ntt(numbers)).equals(numbers)

  import examples.BreezeGeneric.genericDft
  import examples.Zp
  import examples.ZInt

  implicit val anotherRing = new Zp(3329)
  val omega = ZInt(17)
  val data = numbers.map(ZInt(_))
  println(genericDft(data, omega))


}
