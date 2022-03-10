package Chainsaw.algos

import breeze.linalg.{min, norm}
import breeze.math.Semiring
import breeze.math.Field

object AlgebraicStructures {

  case class ZInt(value: Int)

  case class Zp(modulus: Int) extends Semiring[ZInt] { // semiring Zp
    override def zero = ZInt(0)

    override def one = ZInt(1)

    override def +(a: ZInt, b: ZInt) = ZInt((a.value + b.value) % modulus)

    override def *(a: ZInt, b: ZInt) = ZInt((a.value * b.value) % modulus)

    override def ==(a: ZInt, b: ZInt) = a.value == b.value

    override def !=(a: ZInt, b: ZInt) = a.value != b.value
  }

  case class MPInt(value:Int)

  case class MinPlus(max:Int) extends Semiring[MPInt] {

    override def zero = MPInt(max) // min(x, max) = x

    override def one = MPInt(0) // x + 0 = x

    override def +(a: MPInt, b: MPInt) = MPInt(min(a.value, b.value))

    override def *(a: MPInt, b: MPInt) = MPInt(a.value + b.value)

    override def ==(a: MPInt, b: MPInt) = a.value == b.value

    override def !=(a: MPInt, b: MPInt) = a.value != b.value
  }

  // finite field with 2 elements
  case class F2() extends Field[Boolean] {

    override def /(a: Boolean, b: Boolean) = ???

    override def pow(a: Boolean, b: Boolean) = ???

    override def -(a: Boolean, b: Boolean) = ???

    override def %(a: Boolean, b: Boolean) = ???

    override implicit val normImpl: norm.Impl[Boolean, Double] = _

    override def zero = ???

    override def one = ???

    override def +(a: Boolean, b: Boolean) = ???

    override def *(a: Boolean, b: Boolean) = ???

    override def ==(a: Boolean, b: Boolean) = ???

    override def !=(a: Boolean, b: Boolean) = ???
  }
}


