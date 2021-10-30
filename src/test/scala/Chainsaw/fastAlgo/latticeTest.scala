package Chainsaw.fastAlgo

import Chainsaw.DSPRand
import Chainsaw.fastAlgo.lattice._
import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class latticeTest extends AnyFlatSpec {

  val p = 3329 // 3329 = 13 * 256 + 1
  val n = 256
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

    logger.info(s"\n${ret0.mkString(" ")}\n${ret1.mkString(" ")}")
    assert(ret0.diff(ret1).isEmpty)
    ret0
  }

  val f, g = getTestCase(n)
  val a, b = getTestCase(n / 2)
  "the algo to accelerate polynomial multiplication on ideal lattice" should "have a correct decomposition" in verifyDecomposition(f, g, 256)

  it should "be transformed by NTT correctly" in assert(a.diff(INTT(NTT(a))).isEmpty)
  it should "be implemented as cyclic convolution correctly" in assert(CCByNTT(a, b).diff(cyclicConvolution(a, b)).isEmpty)
  it should "be implemented as negative wrapped convolution correctly" in assert(NWCByNTT(a, b).diff(NWC(a, b)).isEmpty)

}
