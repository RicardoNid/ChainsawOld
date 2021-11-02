package Chainsaw

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._
import rings.primes._

import rings.poly.univar._

// utilities of cryptography
package object crypto {

  // utils of poly on Zp64
  implicit class polyZpUtil(poly: UnivariatePolynomialZp64) {
    def getCoeffs: Seq[Long] = (0 to poly.degree()).map(poly.get(_))
  }

  def toPolyExpression(coeffs: Seq[Long], variable: String): String = coeffs.zipWithIndex.map { case (coeff, degree) => s"$coeff*$variable^$degree" }.mkString("+")

  implicit class polyRingZpUtil(polyRing: UnivariateRingZp64) {
    def fromCoeffs(coeffs: Seq[Long]): UnivariatePolynomialZp64 = polyRing(toPolyExpression(coeffs, polyRing.variable))

    /** implement linear convolution as polynomial multiplication
     */
    def linearConvolution(f: Seq[Long], g: Seq[Long]): Seq[Long] = (fromCoeffs(f) * fromCoeffs(g)).getCoeffs

    /** implement cyclic convolution as polynomial modular multiplication
     */
    def cyclicConvolution(f: Seq[Long], g: Seq[Long]): Seq[Long] = {
      require(f.size == g.size)
      (fromCoeffs(f) * fromCoeffs(g) % polyRing(s"x^${f.size}-1")).getCoeffs
    }

    /** implement negative wrapped convolution as polynomial modular multiplication
     */
    def negativeWrappedConvolution(f: Seq[Long], g: Seq[Long]): Seq[Long] = {
      require(f.size == g.size)
      (fromCoeffs(f) * fromCoeffs(g) % polyRing(s"x^${f.size}+1")).getCoeffs
    }
  }

  // utils of Zp64
  implicit class ZpUtil(ring: Ring[Long]) {

    def p: Long = ring.cardinality().longValue()

    def isCorrect(value:Long): Boolean = value == ring(value)

    def getNthRoot(N: Int): Long = {
      require(SmallPrimes.isPrime(p.toInt) && (p - 1) % N == 0, "p is prime & p - 1 % N <=> N-th root of  exists") // TODO: is that true?
      val ret = (2 until p.toInt).filter { root =>
        ring.pow(root, N) == ring(1) && // N-th root
          (1 to N).map(ring.pow(root, _)).distinct.size == N
      }.head // TODO: which is the "best"?
      ret
    }

    def inverseOf(value:Long): Long = ring.pow(value, -1)
  }


}


