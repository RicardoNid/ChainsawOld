package Chainsaw.fastAlgo

import Chainsaw.{DSPRand, _}
import Chainsaw.fastAlgo.lattice._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.scalatest.flatspec.AnyFlatSpec

class latticeTest extends AnyFlatSpec {

  val p = 3329 // 3329 = 13 * 256 + 1
  val k = 13
  val n = 256 // is not the "256" above, the 256 above is 1 << 8, they're the same by coincident
  implicit val polyRing = UnivariateRingZp64(p, "x")
  implicit val cfRing = polyRing.cfRing

  def getTestCase(n: Int) = (0 until n).map(_ => DSPRand.nextInt(p * 10)).map(cfRing(_))

  def verifyDecomposition(coeffsF: Seq[Long], coeffsG: Seq[Long], N: Int)(implicit polyRing: PolyRing) = {

    require(coeffsF.size == N, coeffsG.size == N)
    val f = fromCoeff(coeffsF)
    val g = fromCoeff(coeffsG)
    val even = (0 until N / 2).map(_ * 2)
    val odd = (0 until N / 2).map(_ * 2 + 1)

    // sub polys
    val fEven = fromCoeff(even.map(coeffsF(_)))
    val fOdd = fromCoeff(odd.map(coeffsF(_)))
    val gEven = fromCoeff(even.map(coeffsG(_)))

    val gOddCoeffs = odd.map(coeffsG(_))
    val gOdd = fromCoeff(gOddCoeffs)
    val gOddPrime = fromCoeff(-gOddCoeffs.last +: gOddCoeffs.init) // = gOdd * polyRing.`x` % modulo128

    val modulo = polyRing(s"x^$N + 1")
    val moduloHalf = polyRing(s"x^${N / 2} + 1")

    val ret0 = getCoeffs(f * g % modulo) // the golden is calculated by purely "naive" method

    val ret1 = {
      val evens = getCoeffs(((fEven * gEven) + (fOdd * gOddPrime)) % moduloHalf)
      val odds = getCoeffs(((fOdd * gEven) + (fEven * gOdd)) % moduloHalf)
      evens.zip(odds).flatMap { case (even, odd) => Seq(even, odd) } // "riffle shuffle"
    }

    logger.debug(s"decomposition result: \n${ret0.mkString(" ")}\n${ret1.mkString(" ")}")
    assert(ret0.diff(ret1).isEmpty)
    ret0
  }

  val f, g = getTestCase(n)
  val a, b = getTestCase(n / 2)
  val smallA, smallB = getTestCase(8) // for debugging

  val aAndBs = (0 until 50).map(_ => Seq(DSPRand.nextInt(3328), DSPRand.nextInt(3328)))
  val cs = (0 until 100).map(_ => DSPRand.nextInt(3328 * 3328))
  "the algo" should "make sure 256-point polyMult can be implemented by four 128-point polyMults" in verifyDecomposition(f, g, 256)

  it should "make sure that K2RED = k^2 * c % p" in assert(cs.forall(c => cfRing(K2RED(c, p)) == cfRing(13 * 13 * c)))
  it should "make sure that MULT-K2RED = (k^2 * a * b) % p" in assert(aAndBs.forall(aAndB => cfRing(K2RED(aAndB(0) * aAndB(1), 3329)) == cfRing(K2Mult(aAndB(0), aAndB(1)))))
  it should "make sure that KNTT = NTT" in assert(NTT(a).diff(KNTT(a)).isEmpty)
  it should "make sure that KNTT = NTT at a different size" in assert(NTT(smallA).diff(KNTT(smallA)).isEmpty)
  it should "make sure that NTT-INTT are inverse of each other" in assert(a.diff(INTT(NTT(a))).isEmpty)
  it should "make sure that fast NTT is still NTT" in assert(NTT(a).sorted.diff(fastNTT(a).sorted).isEmpty) // only require them to be the same as sets, as the order is different
  it should "make sure that fast NTT is still NTT at a different size" in assert(NTT(smallA).sorted.diff(fastNTT(smallA).sorted).isEmpty) // only require them to be the same as sets, as the order is different
  it should "make sure that fast NTT-INTT are inverse of each other" in assert(a.diff(fastNTT(fastINTT(a))).isEmpty)
  it should "make sure that CCByNTT is still cyclic convolution" in assert(CCByNTT(a, b).diff(cyclicConvolution(a, b)).isEmpty)
  it should "make sure that NWCByNTT is still negative wrapped convolution correctly" in assert(NWCByNTT(a, b).diff(NWC(a, b)).isEmpty)

}
