package Chainsaw.crypto.lattice

import Chainsaw.DFG.FFTArch._
import Chainsaw.ringsUtils._
import Chainsaw.crypto.FastAlgos.{CCByNTT, NWCByNTT, p2pMult}
import Chainsaw.crypto.lattice.Kyber.KNTT
import Chainsaw.crypto._
import Chainsaw.{ChainsawRand, crypto, logger}
import cc.redberry.rings.scaladsl.{Ring, UnivariateRingZp64}
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

class HuaweiKyberTest extends AnyFlatSpec {

  val doSynths = false

  // huawei configurations p = 3329, polySize = 256
  val p = 3329 // 3329 = 13 * 256 + 1
  val k = 13
  val k2: Int = k * k
  val polySize = 256 // is not the "256" above, the 256 above is 1 << 8, they're the same by coincident
  implicit val polyRing: UnivariateRingZp64 = UnivariateRingZp64(p, "x")
  implicit val cfRing: Ring[Long] = polyRing.cfRing

  def getTestCase(n: Int): Seq[Long] = (0 until n).map(_ => ChainsawRand.nextInt(p).toLong)

  val f, g = getTestCase(polySize)
  val cs: Seq[Int] = (0 until 100).map(_ => ChainsawRand.nextInt((p - 1) * (p - 1) + 1))

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
  import HuaweiKyber._
  // TODO: test method should support int/long as a replacement of bigint
  // TODO: exhaustive test of KRED

  val testProducts: Seq[BigInt] = (0 until 10000).map(_ => BigInt(ChainsawRand.nextInt((p - 1) * (p - 1) + 1))) // 10000-point random test
  val testProductsExhaustive: Seq[BigInt] = (0 to (p - 1) * (p - 1)).map(BigInt(_))
  val randomOmega = ChainsawRand.nextInt(p)
  val testPairs: Seq[Seq[BigInt]] = (0 until 10000).map(_ => BigInt(ChainsawRand.nextInt(p))).grouped(2).toSeq.map(vec => Seq(vec(0), vec(1), BigInt(randomOmega)))

  "KMultMod" should "pass the random test" in {
    val golden = testPairs.map(vec => (vec(0) * vec(1) * k2) % p).map(cfRing(_))
    testDSPNode(kMultModNode, Seq(opWidth, opWidth), testPairs, golden)
  }

  "KAddMod" should "pass the random test" in {
    val golden = testPairs.map(pair => cfRing(pair(0) + pair(1)))
    testDSPNode(BinaryHardware(kAddMod,  delay = 1 cycles).asDeviceNode("kAddMod"), Seq(opWidth, opWidth), testPairs, golden)
  }

  "KSubMod" should "pass the random test" in {
    val golden = testPairs.map(pair => cfRing(pair(0) - pair(1)))
    testDSPNode(BinaryHardware(kSubMod,  delay = 1 cycles).asDeviceNode("kSubMod"), Seq(opWidth, opWidth), testPairs, golden)
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

  if (doSynths) {
    "all these operators" should "synth correctly" in {
      synthDSPNode(kMultModNode, Seq(opWidth, opWidth))
      synthDSPNode(ctButterflyNode, Seq.fill(3)(opWidth))
      synthDSPNode(gsButterflyNode, Seq.fill(3)(opWidth))
    }
  }

  val testCount = 10

  // hardware impl of fastNTT by a butterfly network
  val nttDFG: DFGGraph[UInt] = ButterflyGen(ctButterflyNode, gsButterflyNode, N, DIF, inverse = false, coeffGen, 12 bits, 1).getGraph
  // we use DIT here to process a bit-reversed sequence as we won't reorder the result of NTT
  val inttDFG: DFGGraph[UInt] = ButterflyGen(ctButterflyNode, gsButterflyNode, N, DIT, inverse = true, coeffGen, 12 bits, 1).getGraph

  val nttTestCase: Seq[Seq[Long]] = Seq.tabulate(testCount, N)((_, _) => ChainsawRand.nextInt(p).toLong)
  val knttTestCase: Seq[Seq[BigInt]] = nttTestCase.flatten.map(value => BigInt(cfRing(value * k2Inverse))).grouped(N).toSeq

  val nttGolden: Seq[Seq[Long]] = nttTestCase.map(NTT)
  val inttGolden: Seq[Seq[Long]] = nttTestCase.map(seq => INTT(nttAlgo.bitReverse(seq))) // the input is bit-reversed

  val knttGolden: Seq[Seq[BigInt]] = nttGolden.flatten.map(value => BigInt(cfRing(value * k2Inverse))).grouped(N).toSeq
  val kinttGolden: Seq[Seq[BigInt]] = inttGolden.flatten.map(value => BigInt(cfRing(value * k2Inverse * N))).grouped(N).toSeq

  "the KNTT hardware" should "pass the random test" in {

    testDSPNode[UInt, Seq[BigInt], BigInt](
      nttDFG.asNode("ntt", log2Up(N) * 5 cycles), Seq.fill(N)(12 bits),
      knttTestCase,
      knttGolden.flatten)
  }

  if (doSynths) it should "synth correctly" in synthDSPNode[UInt](nttDFG.asNode("ntt", log2Up(N) * 5 cycles), Seq.fill(N)(12 bits))

  implicit def long2UInt: (Long, BitCount) => UInt = (value: Long, _: BitCount) => U(value, 12 bits) // TODO:

  "the IKNTT hardware" should "pass the random test" in {
    testDSPNode[UInt, Seq[BigInt], BigInt](
      inttDFG.asNode("intt", log2Up(N) * 5 cycles), Seq.fill(N)(12 bits),
      knttTestCase,
      kinttGolden.flatten)
  }
}
