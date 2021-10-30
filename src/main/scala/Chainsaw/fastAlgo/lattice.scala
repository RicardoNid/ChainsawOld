package Chainsaw.fastAlgo

import Chainsaw._
import cc.redberry.rings
import cc.redberry.rings.primes._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import spinal.core._

object lattice {

  // HUAWEI coeff: p=3329, n=256

  type PolyRing = UnivariateRingZp64
  type CfRing = Ring[Long]
  type Poly = UnivariatePolynomialZp64

  case class Term(coeff: Long, order: Long)

  def getOmega(cfRing: CfRing, n: Int) = {
    val p = cfRing.cardinality().intValue()
    (2 until p).filter { root => // TODO: is it unique?
      cfRing.pow(root, n) == cfRing(1) && // n-th root
        (1 to n).map(cfRing.pow(root, _)).distinct.size == n && // TODO: is this necessary?
        (2 until p).exists(cfRing.pow(_, 2) == cfRing(root)) // phi exist
    }.head
  }

  def getPhi(cfRing: CfRing, n: Int) = {
    val p = cfRing.cardinality().intValue()
    val omega = getOmega(cfRing, n)
    (2 until p).filter(cfRing.pow(_, 2) == cfRing(omega)).head // phi exist
  }

  def buildString(terms: Seq[Term], symbol: String) = terms.map(term => s"${term.coeff}*$symbol^${term.order}").mkString("+")

  def getCoeffs(poly: UnivariatePolynomialZp64) = (0 to poly.degree()).map(poly.get(_).toLong)

  def fromCoeff(coeffs: Seq[Long])(implicit ring: UnivariateRingZp64): UnivariatePolynomialZp64 =
    ring(buildString(coeffs.zipWithIndex.map { case (coeff, order) => Term(coeff, order) }, ring.variable))

  def NTT(coeffs: Seq[Long], inverse: Boolean = false)(implicit cfRing: CfRing): Seq[Long] = {

    val N: Int = coeffs.size
    require(isPow2(coeffs.size))
    val inverseN: Long = cfRing.pow(N, -1)
    val omega: Long = getOmega(cfRing, N)

    val ret = (0 until N).map { k =>
      if (!inverse) cfRing(coeffs.zipWithIndex.map { case (value, i) => value * cfRing.pow(omega, i * k) }.sum)
      else cfRing(coeffs.zipWithIndex.map { case (value, i) => value * cfRing.pow(omega, -i * k) }.sum * inverseN)
    }
    val inverseString = if (inverse) "inverse" else ""
    logger.debug(s"$inverseString NTT:\nN:$N, N^-1:$inverseN, omega:$omega\ninput:  ${coeffs.mkString(" ")}\nresult: ${ret.mkString(" ")}")
    ret
  }

  def INTT(coeffs: Seq[Long])(implicit cfRing: CfRing) = NTT(coeffs, true)

  def multP2P(coeffsF: Seq[Long], coeffsG: Seq[Long])(implicit cfRing: CfRing) =
    coeffsF.zip(coeffsG).map { case (fi, gi) => cfRing(fi * gi) }

  def CCByNTT(coeffsF: Seq[Long], coeffsG: Seq[Long])(implicit cfRing: CfRing) =
    INTT(multP2P(NTT(coeffsF), NTT(coeffsG)))

  def cyclicConvolution(coeffsF: Seq[Long], coeffsG: Seq[Long])(implicit polyRing: UnivariateRingZp64) = {
    require(coeffsG.size == coeffsF.size)
    val n = coeffsF.size
    getCoeffs(fromCoeff(coeffsF) * fromCoeff(coeffsG) % polyRing(s"x^$n-1"))
  }

  def NWC(coeffsF: Seq[Long], coeffsG: Seq[Long])(implicit polyRing: UnivariateRingZp64) = {
    require(coeffsG.size == coeffsF.size)
    val n = coeffsF.size
    getCoeffs(fromCoeff(coeffsF) * fromCoeff(coeffsG) % polyRing(s"x^$n+1"))
  }

  /** Implement polynomial multiplication on ideal lattice as negative wrapped convolution, accelerated by NTT
   */
  def NWCByNTT(coeffsF: Seq[Long], coeffsG: Seq[Long])(implicit cfRing: CfRing) = {
    val n = coeffsF.size
    val phi = getPhi(cfRing, n)
    val inversePhi: Long = cfRing.pow(phi, -1)
    val modifiedCoeffsF = coeffsF.zipWithIndex.map{ case (coeff, i) => coeff * cfRing.pow(phi, i)}
    val modifiedCoeffsG = coeffsG.zipWithIndex.map{ case (coeff, i) => coeff * cfRing.pow(phi, i)}
    val ret = CCByNTT(modifiedCoeffsF, modifiedCoeffsG)
    ret.zipWithIndex.map{ case (coeff, i) => coeff * cfRing.pow(inversePhi, i)}.map(cfRing(_))
  }

}
