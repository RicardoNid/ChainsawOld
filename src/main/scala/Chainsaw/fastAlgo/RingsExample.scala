package Chainsaw.fastAlgo

import cc.redberry.rings

import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._

object RingsExample {
  def main(args: Array[String]): Unit = {

    // claim algebra
    val PolyRingOnZ: UnivariateRing[IntZ] = UnivariateRing(Z, "x")
    val poly0: UnivariatePolynomial[IntZ] = PolyRingOnZ("x^2 + 2*x + 1")
    println(Factor(poly0).mkString(" "))
    val poly1: UnivariatePolynomial[IntZ] = PolyRingOnZ("x^3 - 1")
    println(Factor(poly1).mkString(" "))
    val product: UnivariatePolynomial[IntZ] = poly0 * poly1
    val value: IntZ = product.evaluate(3)
    println(value.intValue())

    val Z7 = Zp(7)

    // do arithmetics
    val a = Z7(9)
    val b = Z7(15)
    println(a + b)
    println(a * b)
    println(a.divideAndRemainder(b).mkString(" "))
    println(a % b)
    println(a gcd b)

    /** Lagrange polynomial interpolation formula
     *
     * @param points x -> p(x) pairs
     * @param ring the background polynomial ring of interpolation
     * @tparam Poly the polynomial type
     * @tparam Coef the coefficient ring type
     * @return result polynomial of Lagrange interpolation
     */
    def interpolate[Poly <: IUnivariatePolynomial[Poly], Coef]
    (points: Seq[(Coef, Coef)])
    (implicit ring: IUnivariateRing[Poly, Coef]) = {
      // implicit coefficient ring (setups algebraic operators on type Coef)
      implicit val cfRing: Ring[Coef] = ring.cfRing
      if (!cfRing.isField) throw new IllegalArgumentException
      val betas = points.map(_._1)
      val values = points.map(_._2)
      points.indices.map { i =>
        val indices = points.indices.filterNot(_ == i) // j != i
        val numerator = indices.map(j => ring.`x` - betas(j)).reduce(_ * _)
        val denominator = indices.map(j => betas(i) - betas(j)).reduce(_ * _)
        ring(values(i)) * numerator / denominator // ring(values(i)) regards the coeff as a 0th-order poly
      }.reduce(_ + _)
    }

    implicit val ring = UnivariateRing(Q, "x")
    // provided three points of polynomial x^2 + 1
    println(s"poly: ${interpolate(Seq(Q(0) -> Q(1), Q(1) -> Q(2), Q(-1) -> Q(2)))}")
  }
}
