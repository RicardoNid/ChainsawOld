package Chainsaw.dsl.field

import spinal.core._

class IntField(val width:Int) extends Field[Int] {
  override def add(a: Int, b: Int) = a + b

  override def subtract(a: Int, b: Int) = a - b

  override def multiply(a: Int, b: Int) = a * b

  override def identity(a: Int) = a

  override def zero = 0

  override def one = 1

  override def addH(a: Bits, b: Bits) = (a.asSInt + b.asSInt).resize(width).asBits

  override def subtractH(a: Bits, b: Bits) = (a.asSInt - b.asSInt).resize(width).asBits

  override def multiplyH(a: Bits, b: Bits) = (a.asSInt * b.asSInt).resize(width).asBits

  override def identityH(a: Bits) = a

  override def zeroH = B(0, width bits)

  override def oneH = B(1, width bits)

  override def undefinedH = Bits(width bits)

  override def toHard(coeff: Int) = B(coeff, width bits)
}

object IntField {
  def apply(width:Int): IntField = new IntField(width)
}
