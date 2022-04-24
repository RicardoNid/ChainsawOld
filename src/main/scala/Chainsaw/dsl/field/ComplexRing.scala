package Chainsaw.dsl.field

import breeze.math._
import spinal.core.Bits
import Chainsaw.dsl._

class ComplexRing(integral: Int, fractional: Int) extends MixType[Complex] with RingOp[Complex] {
  override val width = integral + fractional + 1

  def scaling = (1 << fractional).toDouble

  override def toBits(value: Complex) = {
    val toInt: Double => BigInt = x => BigInt((x * scaling).toInt)
    toInt(value.real).toSigned(width) ++ toInt(value.imag).toSigned(width)
  }

  override def fromBits(bits: String) = {
    val (real, imag) = bits.splitAt(width)
    Complex(real.asSigned.toDouble / scaling, imag.asSigned.toDouble / scaling)
  }

  override def add(a: Complex, b: Complex) = a + b

  override def sub(a: Complex, b: Complex) = a - b

  override def mult(a: Complex, b: Complex) = a * b

  // todo: implement complex operators
  override def addH(a: Bits, b: Bits) = a

  override def subH(a: Bits, b: Bits) = a

  override def multH(a: Bits, b: Bits) = a

  override def idH(a: Bits) = a
}

object ComplexRing {
  def apply(integral: Int, fractional: Int): ComplexRing = new ComplexRing(integral, fractional)
}
