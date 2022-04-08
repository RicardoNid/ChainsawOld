package Chainsaw.algos

import breeze.linalg.{min, norm}
import breeze.math.Semiring
import breeze.math.Field

object AlgebraicStructures {

  case class ZpInt(value: Int)

  case class SemiZp(modulus: Int) extends Semiring[ZpInt] { // semiring Zp
    override def zero = ZpInt(0)

    override def one = ZpInt(1)

    override def +(a: ZpInt, b: ZpInt) = ZpInt((a.value + b.value) % modulus)

    override def *(a: ZpInt, b: ZpInt) = ZpInt((a.value * b.value) % modulus)

    override def ==(a: ZpInt, b: ZpInt) = a.value == b.value

    override def !=(a: ZpInt, b: ZpInt) = a.value != b.value
  }

  case class MPInt(value: Int)

  case class MinPlus(max: Int) extends Semiring[MPInt] {

    override def zero = MPInt(max) // min(x, max) = x

    override def one = MPInt(0) // x + 0 = x

    override def +(a: MPInt, b: MPInt) = MPInt(min(a.value, b.value))

    override def *(a: MPInt, b: MPInt) = MPInt(a.value + b.value)

    override def ==(a: MPInt, b: MPInt) = a.value == b.value

    override def !=(a: MPInt, b: MPInt) = a.value != b.value
  }


  case class FFInt(value: Int)(implicit finiteField: FiniteField) {
    require(value >= 0 && value < finiteField.q)
  }

  // finite field with q elements
  case class FiniteField(q: Int) extends Field[FFInt] {

    import cc.redberry.rings

    import rings.poly.PolynomialMethods._
    import rings.scaladsl._
    import syntax._
    import rings.primes._

    implicit val field = this

    // todo: avoid dependencies on rings
    val ring = Zp64(q)

    override def +(a: FFInt, b: FFInt) = FFInt(ring.add(a.value, b.value).toInt)

    override def -(a: FFInt, b: FFInt) = FFInt(ring.subtract(a.value, b.value).toInt)

    override def *(a: FFInt, b: FFInt) = FFInt(ring.multiply(a.value, b.value).toInt)

    // inverse of *
    override def /(a: FFInt, b: FFInt) = FFInt(ring.divide(a.value, b.value).toInt)

    override def pow(a: FFInt, b: FFInt) = FFInt(Seq.fill(b.value)(a.value).product % q)

    override def %(a: FFInt, b: FFInt) = FFInt(ring(a.value % b.value).toInt)

    // todo: implement this correctly
    override implicit val normImpl: norm.Impl[FFInt, Double] = null

    override def zero = FFInt(0)

    override def one = FFInt(1)

    override def ==(a: FFInt, b: FFInt) = a.value == b.value

    override def !=(a: FFInt, b: FFInt) = a.value != b.value

    def getNthRoot(N: Int) = {
      import cc.redberry.rings.primes.BigPrimes
      import cc.redberry.rings.scaladsl.{Zp64, Ring}
      // TODO: better algo
      require(BigPrimes.isPrime(q) && (q - 1) % N == 0, "p is prime & p - 1 % N <=> N-th root of exists") // TODO: true?
      val ring: Ring[Long] = Zp64(q)
      val ret = (2 until q).filter { root =>
        ring.pow(root, N) == ring(1) && // N-th root
          (1 to N).map(ring.pow(root, _)).distinct.size == N
      }.head
      ret
    }
  }
}


