package Chainsaw.crypto.lattice

import Chainsaw.crypto.FastAlgos.{CCByNTT, NWCByNTT}
import Chainsaw.crypto.lattice.Kyber.KNTT
import Chainsaw.crypto._
import Chainsaw.crypto.lattice.HuaweiKyber.gsButterflyNode
import Chainsaw.{DSPRand, crypto, logger}
import cc.redberry.rings.scaladsl.{Ring, UnivariateRingZp64}
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

class HuaweiKyberTest extends AnyFlatSpec {

  // huawei configurations p = 3329, polySize = 256
  val p = 3329 // 3329 = 13 * 256 + 1
  val k = 13
  val k2: Int = k * k
  val polySize = 256 // is not the "256" above, the 256 above is 1 << 8, they're the same by coincident
  implicit val polyRing: UnivariateRingZp64 = UnivariateRingZp64(p, "x")
  implicit val cfRing: Ring[Long] = polyRing.cfRing

  def getTestCase(n: Int): Seq[Long] = (0 until n).map(_ => DSPRand.nextInt(p).toLong)

  val f, g = getTestCase(polySize)
  val cs: Seq[Int] = (0 until 100).map(_ => DSPRand.nextInt((p - 1) * (p - 1) + 1))

  behavior of "software"

  logger.info("testing huawei NTT software")

  "the software algorithms" should "make sure 256-point polyMult can be implemented by four 128-point polyMults" in {
    val N = 256
    val even = (0 until N / 2).map(_ * 2)
    val odd = (0 until N / 2).map(_ * 2 + 1)
    val fEven = even.map(f(_))
    val fOdd = odd.map(f(_))
    val gEven = even.map(g(_))
    val gOdd = odd.map(g(_))
    val gOddPrime = -gOdd.last +: gOdd.init
    val nwc = polyRing.negativeWrappedConvolution(_, _)
    val ret0 = nwc(f, g)
    val ret1evens = nwc(fEven, gEven).zip(nwc(fOdd, gOddPrime)).map { case (a, b) => cfRing(a + b) }
    val ret1odds = nwc(fOdd, gEven).zip(nwc(fEven, gOdd)).map { case (a, b) => cfRing(a + b) }
    val ret1 = ret1evens.zip(ret1odds).flatMap { case (even, odd) => Seq(even, odd) } // "riffle shuffle"
    assert(ret0.diff(ret1).isEmpty)
  }

  val N = 128
  val a, b = getTestCase(N)
  val nttAlgo: NTT = crypto.NTT(cfRing, N)
  val NTT: Seq[Long] => Seq[Long] = nttAlgo.NTT(_, fast = false)
  val FNTT: Seq[Long] => Seq[Long] = nttAlgo.NTT(_)
  val INTT: Seq[Long] => Seq[Long] = nttAlgo.INTT(_, fast = false)
  val FINTT: Seq[Long] => Seq[Long] = nttAlgo.INTT(_)
  val K2RED: Int => Long = crypto.ModularReduction.K2RED(_, cfRing)

  it should "make sure that K2RED = k^2 * c % p" in assert(cs.forall(c => cfRing(K2RED(c)) == cfRing(k2 * c)))
  it should "make sure that KNTT = NTT" in assert(NTT(a).diff(KNTT(a)).isEmpty)
  it should "make sure that NTT-INTT are inverse of each other" in assert(a.diff(INTT(NTT(a))).isEmpty)
  it should "make sure that fast NTT is still NTT" in assert(NTT(a).sorted.diff(FNTT(a).sorted).isEmpty) // only require them to be the same as sets, as the order is different
  it should "make sure that fast NTT-INTT are inverse of each other" in assert(a.diff(FNTT(FINTT(a))).isEmpty)
  it should "make sure that CCByNTT is still cyclic convolution" in assert(CCByNTT(a, b).diff(polyRing.cyclicConvolution(a, b)).isEmpty)
  it should "make sure that NWCByNTT is still negative wrapped convolution correctly" in assert(NWCByNTT(a, b).diff(polyRing.negativeWrappedConvolution(a, b)).isEmpty)

  behavior of "hardware"

  logger.info("testing huawei NTT hardware")

  val opWidth: BitCount = 12 bits

  import Chainsaw.DFG._
  import HuaweiKyber.{ctButterflyNode, kAddMod, kMultModNode, kSubMod}
  // TODO: test method should support int/long as a replacement of bigint
  // TODO: exhaustive test of KRED

  val testProducts: Seq[BigInt] = (0 until 10000).map(_ => BigInt(DSPRand.nextInt((p - 1) * (p - 1) + 1))) // 10000-point random test
  val testProductsExhaustive: Seq[BigInt] = (0 to (p - 1) * (p - 1)).map(BigInt(_))
  val randomOmega = DSPRand.nextInt(p)
  val testPairs: Seq[Seq[BigInt]] = (0 until 10000).map(_ => BigInt(DSPRand.nextInt(p))).grouped(2).toSeq.map(vec => Seq(vec(0), vec(1), BigInt(randomOmega)))

  "KMultMod" should "pass the random test" in {
    val golden = testPairs.map(vec => (vec(0) * vec(1) * k2) % p).map(cfRing(_))
    testDSPNode(kMultModNode, Seq(opWidth, opWidth), testPairs, golden)
  }

  "KAddMod" should "pass the random test" in {
    val golden = testPairs.map(pair => cfRing(pair(0) + pair(1)))
    testDSPNode(BinaryNode(kAddMod, "kAddMod", delay = 1 cycles), Seq(opWidth, opWidth), testPairs, golden)
  }

  "KSubMod" should "pass the random test" in {
    val golden = testPairs.map(pair => cfRing(pair(0) - pair(1)))
    testDSPNode(BinaryNode(kSubMod, "kSubMod", delay = 1 cycles), Seq(opWidth, opWidth), testPairs, golden)
  }

  "CTBF" should "pass the random test" in {
    val golden = testPairs.map { vec =>
      val (u, v, omega) = (vec(0), vec(1), vec(2))
      val vw = cfRing(k2 * v * omega)
      (cfRing(u + vw), cfRing(u - vw))
    }.flatMap(pair => Seq(pair._1, pair._2))

    val result: Seq[BigInt] = testDSPNode(ctButterflyNode, Seq.fill(3)(opWidth), testPairs, golden).asInstanceOf[Seq[BigInt]]
    result.map(_.toLong).forall(cfRing.isCorrect)
  }

  "GSBF" should "pass the random test" in {
    val golden = testPairs.map { vec =>
      val (u, v, omega) = (vec(0), vec(1), vec(2))
      (cfRing(u + v), cfRing(k2 * (u - v) * omega))
    }.flatMap(pair => Seq(pair._1, pair._2))

    val result: Seq[BigInt] = testDSPNode(gsButterflyNode, Seq.fill(3)(opWidth), testPairs, golden).asInstanceOf[Seq[BigInt]]
    result.map(_.toLong).forall(cfRing.isCorrect)
  }

  "all these operators" should "synth correctly" in {
    synthDSPNode(kMultModNode, Seq(opWidth, opWidth))
    synthDSPNode(ctButterflyNode, Seq.fill(3)(opWidth))
    synthDSPNode(gsButterflyNode, Seq.fill(3)(opWidth))
  }

}