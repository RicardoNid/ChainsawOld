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
    val ret = (2 until p).filter { root => // TODO: is it unique?
      cfRing.pow(root, n) == cfRing(1) && // n-th root
        (1 to n).map(cfRing.pow(root, _)).distinct.size == n && // TODO: is this necessary?
        (2 until p).exists(cfRing.pow(_, 2) == cfRing(root)) // phi exist
    }.head
    require(cfRing.pow(ret, n) == 1) // make sure that omega is the n-th root
    ret
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

  def fastNTT(coeffs: Seq[Long], inverse: Boolean = false)(implicit cfRing: CfRing): Seq[Long] = {

    def ctButterfly(u: Long, v: Long, omega: Long) = (cfRing(u + omega * v), cfRing(u - omega * v)) // backward

    def gsButterfly(u: Long, v: Long, omega: Long) = (cfRing(u + v), cfRing((u - v) * omega)) // forward

    val N: Int = coeffs.size
    require(isPow2(coeffs.size))
    val inverseN: Long = cfRing.pow(N, -1)
    val omega: Long = getOmega(cfRing, N)

    def buildStage(dataIns: Seq[Long]) = {
      logger.debug(s"input to stage:    ${dataIns.mkString(" ")}")
      val (up, down) = dataIns.splitAt(dataIns.size / 2)
      val step = N / dataIns.size
      val temp = up.zip(down).zipWithIndex.map { case ((u, v), i) =>
        if (!inverse) gsButterfly(u, v, cfRing.pow(omega, step * i))
        else ctButterfly(u, v, cfRing.pow(omega, -step * i))
      }
      val ret = temp.map(_._1) ++ temp.map(_._2)
      logger.debug(s"output from stage: ${ret.mkString(" ")}")
      ret
    }

    def buildRecursively(dataIns: Seq[Long]): Seq[Long] = {
      if (dataIns.size == 1) dataIns
      else {
        if (!inverse) {
          val temp = buildStage(dataIns)
          val (up, down) = temp.splitAt(dataIns.size / 2)
          buildRecursively(up) ++ buildRecursively(down)
        } else {
          val (up, down) = dataIns.splitAt(dataIns.size / 2)
          val temp = buildRecursively(up) ++ buildRecursively(down)
          buildStage(temp)
        }
      }
    }

    val ret = if(!inverse) buildRecursively(coeffs) else buildRecursively(coeffs).map(value => cfRing(value * inverseN))
    val inverseString = if (inverse) "inverse" else ""
    logger.debug(s"$inverseString fast NTT:\nN:$N, N^-1:$inverseN, omega:$omega\ninput:  ${coeffs.mkString(" ")}\nresult: ${ret.mkString(" ")}")
    ret
  }

  def fastINTT(coeffs: Seq[Long])(implicit cfRing: CfRing): Seq[Long] = fastNTT(coeffs, inverse = true)

  // k = 13
  def K2Mult(a: Long, b: Long)(implicit cfRing: CfRing) = cfRing(a * b * 13 * 13)

  def KNTT(coeffs: Seq[Long], inverse: Boolean = false)(implicit cfRing: CfRing): Seq[Long] = {

    val N: Int = coeffs.size
    require(isPow2(coeffs.size))
    val inverseN: Long = cfRing.pow(N, -1)
    val omega: Long = getOmega(cfRing, N)

    val k2 = 13 * 13
    val k2Inverse = cfRing.pow(k2, -1)

    val precomputedCoeff = coeffs.map(_ * k2Inverse)

    val ret = (0 until N).map { k =>
      if (!inverse) cfRing(precomputedCoeff.zipWithIndex.map { case (value, i) => K2Mult(value, cfRing.pow(omega, i * k) * k2Inverse) }.sum)
      else cfRing(precomputedCoeff.zipWithIndex.map { case (value, i) => K2Mult(value, cfRing.pow(omega, -i * k) * k2Inverse) }.sum * inverseN)
    }

    val finalRet = ret.map(value => cfRing(value * k2))

    val inverseString = if (inverse) "inverse" else ""
    logger.debug(s"$inverseString NTT:\nN:$N, N^-1:$inverseN, omega:$omega\ninput:  ${coeffs.mkString(" ")}\nresult: ${finalRet.mkString(" ")}")

    finalRet
  }

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
    val modifiedCoeffsF = coeffsF.zipWithIndex.map { case (coeff, i) => coeff * cfRing.pow(phi, i) }
    val modifiedCoeffsG = coeffsG.zipWithIndex.map { case (coeff, i) => coeff * cfRing.pow(phi, i) }
    val ret = CCByNTT(modifiedCoeffsF, modifiedCoeffsG)
    ret.zipWithIndex.map { case (coeff, i) => coeff * cfRing.pow(inversePhi, i) }.map(cfRing(_))
  }

  def K2RED(c: Int, q: Int) = { // TODO: generalize this for different q

    val m = (q - 1).toBinaryString.reverse.takeWhile(_ == '0').size
    val k = (q - 1) / (1 << m)
    require(q == k * (1 << m) + 1) // q = k * 2^m + 1

    val factor = 1 << m
    val ring = Zp(factor)

    val cl = c % factor
    val ch = c / factor
    val cPrime = k * cl - ch

    val cPrimeSize = cPrime.toBinaryString.dropWhile(_ == '1').size + 1

    val cPrimel = BigInt(cPrime.toBinaryString.takeRight(8), 2) // c7 -> 0
    val cPrimeh = scala.math.floor(cPrime / factor.toDouble).toInt // c15 -> c8
    val cPrime2 = k * cPrimel - cPrimeh

    logger.debug(s"K2RED: c $c, cl $cl, ch $ch, cPrime $cPrime, cPrimeh $cPrimeh, cPrimel $cPrimel, ret $cPrime2")
    require(cPrimeSize <= 17, s"$ch, $cl") // extra bit(16 + 1) for sign extension
    require(ring(cPrimel) == ring(cPrime % factor))
    require(cPrimeh * factor + cPrimel == cPrime)

    if (cPrime2 >= 0) cPrime2 else cPrime2 + q
  }
}
