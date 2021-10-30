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

    println(s"Z is a field: ${Z.isField}")
    println(s"Z/4 is a field: ${Zp(4).isField}")
    println(s"GF(3) is a field: ${GF(3, 1).isField}")
    println(s"Z/7 is a field: ${Zp(7).isField}")


    case class Term(coeff: Long, order: Long)

    def buildString(terms: Seq[Term], symbol: String) = {
      terms.map(term => s"${term.coeff}*$symbol^${term.order}").mkString("+")
    }

    import cc.redberry.rings.primes._
    println(SmallPrimes.isPrime(3329))
    val zp3329 = Zp(3329)
    println(s"power ${zp3329.pow(2, 256)}")

    /** HUAWEI,Competition [[https://cpipc.acge.org.cn//cw/detail/10/2c90800c78715fdd0178a1cb74720c89]]Problem 3
     */
    def evaluateHUAWEI(poly0: Seq[Term], poly1: Seq[Term], ret: Seq[Term]) = {
      val polyRing = UnivariateRingZp64(3329, "x")
      val modulo = polyRing("x^256 + 1")

      val golden = polyRing(buildString(poly0, "x")) * polyRing(buildString(poly1, "x")) % modulo
      val goldenTerms = (0 until 255).map(i => i -> golden.get(i)).filterNot(_._2 == 0).map{ case (order, coeff) => Term(coeff, order)}

      println("evaluate HUAWEI")
      println(s"yours:  ${ret.mkString(" ")}")
      println(s"golden: ${goldenTerms.mkString(" ")}")
//      assert(ret.diff(goldenTerms).isEmpty, s"${ret.diff(goldenTerms)}")
    }

    val thePoly1 = Seq(Term(278,245), Term(1,0))
    val thePoly2 = Seq(Term(213,399), Term(2,0))
    val result = Seq(Term(2,0), Term(2621,132), Term(3116,143), Term(556,246))
    evaluateHUAWEI(thePoly1, thePoly2, result)

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
     * @param ring   the background polynomial ring of interpolation
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
