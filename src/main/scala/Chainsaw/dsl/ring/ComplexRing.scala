package Chainsaw.dsl.ring

import breeze.math._
import spinal.core.Bits
import Chainsaw.dsl._

class ComplexRing(integral: Int, fractional: Int) extends MixType[Complex] with RingOp[Complex] {
  override val width = (integral + fractional + 1) * 2

  def scaling = (1 << fractional).toDouble

  override def toBits(value: Complex) = {
    val toInt: Double => BigInt = x => BigInt((x * scaling).toInt)
    toInt(value.real).toSigned(width / 2) ++ toInt(value.imag).toSigned(width / 2)
  }

  override def fromBits(bits: String) = {
    val (real, imag) = bits.splitAt(width / 2)
    Complex(real.asSigned.toDouble / scaling, imag.asSigned.toDouble / scaling)
  }

  override def add(a: Complex, b: Complex) = a + b

  override def sub(a: Complex, b: Complex) = a - b

  override def mult(a: Complex, b: Complex) = a * b

  // TODO: not implemented
  override val addH = new HardOp2(
    op = (a: Bits, b: Bits) => a,
    latency = 0
  )
  override val subH = new HardOp2(
    op = (a: Bits, b: Bits) => a,
    latency = 0
  )
  override val multH = new HardOp2(
    op = (a: Bits, b: Bits) => a,
    latency = 0
  )
}

object ComplexRing {
  def apply(integral: Int, fractional: Int): ComplexRing = new ComplexRing(integral, fractional)
}
