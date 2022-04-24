package Chainsaw.dsl.field

import spinal.core.Bits
import Chainsaw.dsl._

class UIntRing(val width:Int) extends MixType[Int] with RingOp[Int] {

  override def toBits(value: Int) = BigInt(value).toUnsigned(width)

  override def fromBits(bits: String) = bits.asUnsigned.toInt

  override def add(a: Int, b: Int) = a + b

  override def sub(a: Int, b: Int) = a - b

  override def mult(a: Int, b: Int) = a * b

  override def addH(a: Bits, b: Bits) = (a.asUInt +^ b.asUInt).resize(width).asBits

  override def subH(a: Bits, b: Bits) = (a.asUInt -^ b.asUInt).resize(width).asBits

  override def multH(a: Bits, b: Bits) = (a.asUInt * b.asUInt).resize(width).asBits

  override def idH(a: Bits) = a
}

object UIntRing {
  def apply(width:Int): UIntRing = new UIntRing(width)
}
