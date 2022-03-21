package Chainsaw.crypto

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import cc.redberry.rings.scaladsl.{Ring, UnivariateRingZp64}
import org.scalatest.flatspec.AnyFlatSpec

class NTTTest extends AnyFlatSpec {

  val p                                     = 3329
  implicit val polyRing: UnivariateRingZp64 = UnivariateRingZp64(p, "x")
  implicit val cfRing: Ring[Long]           = polyRing.cfRing

  val testSize            = 128
  val testCase: Seq[Long] = (0 until testSize).map(_ => ChainsawRand.nextInt(p).toLong)

  val nttAlgo: NTT = NTT(cfRing, testSize)

  "NTT" should "be a inverse of INTT" in assert(nttAlgo.INTT(nttAlgo.NTT(testCase)).diff(testCase).isEmpty)
  it should "make sure that fast-NTT is implemented correctly" in {
    assert(nttAlgo.INTT(testCase).sorted.diff(nttAlgo.INTT(testCase, fast = false).sorted).isEmpty)
    assert(nttAlgo.NTT(testCase).sorted.diff(nttAlgo.NTT(testCase, fast = false).sorted).isEmpty)
  }

}
