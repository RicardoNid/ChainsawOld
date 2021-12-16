package Chainsaw.algos

import breeze.math.Semiring

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
}


