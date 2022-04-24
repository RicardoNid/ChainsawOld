package Chainsaw.dsl.field

import Chainsaw.dsl._
import spinal.core._

case class FiniteRing(modulo: Int)
  extends MixType[FiniteInt]
    with RingOp[FiniteInt] {

  require(isPow2(modulo))

  override val width = log2Up(modulo)

  override def toBits(value: FiniteInt) = {
    require(value.value < modulo, s"value $value is outside the set of finite field with modulo $modulo")
    BigInt(value.value).toUnsigned(width)
  }

  override def fromBits(bits: String) = FiniteInt(bits.asUnsigned.toInt)

  override def add(a: FiniteInt, b: FiniteInt) = (a.value + b.value) % modulo

  override def sub(a: FiniteInt, b: FiniteInt) = (a.value + b.value)

  override def mult(a: FiniteInt, b: FiniteInt) = (a.value + b.value)

  override def addH(a: Bits, b: Bits) = (a.asUInt + b.asUInt).asBits

  override def subH(a: Bits, b: Bits) = (a.asUInt - b.asUInt).asBits

  override def multH(a: Bits, b: Bits) = (a.asUInt * b.asUInt).resize(width).asBits

  override def idH(a: Bits) = a
}
