package Chainsaw.dsl.field

import Chainsaw.dsl._
import spinal.core._

import scala.util.Random

class SIntRing(val width:Int) extends MixType[Int] with RingOp[Int] {

  override def toBits(value: Int) = BigInt(value).toSigned(width)

  override def fromBits(bits: String) = bits.asSigned.toInt

  override def add(a: Int, b: Int) = a + b

  override def sub(a: Int, b: Int) = a - b

  override def mult(a: Int, b: Int) = a * b

  override def addH(a: Bits, b: Bits) = (a.asSInt +^ b.asSInt).resize(width).asBits

  override def subH(a: Bits, b: Bits) = (a.asSInt -^ b.asSInt).resize(width).asBits

  override def multH(a: Bits, b: Bits) = (a.asSInt * b.asSInt).resize(width).asBits

  override def idH(a: Bits) = a
}

object SIntRing {
  def apply(width:Int): SIntRing = new SIntRing(width)
}
