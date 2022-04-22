package Chainsaw.dsl.field

import Chainsaw._
import spinal.core._

import scala.util.Random

class IntField(val width:Int) extends Field[Int] {

  override def toBits(value: Int) = value.toBinaryString.padToLeft(width, '0')

  override def fromBits(bits: String) = BigInt(bits, 2).toInt

  override def add(a: Int, b: Int) = a + b

  override def subtract(a: Int, b: Int) = a - b

  override def multiply(a: Int, b: Int) = a * b

  override def identity(a: Int) = a

  override def zero = 0

  override def one = 1

  override def random = Random.nextInt(1 << (width / 2))

  override def toBigInt(value: Int) = BigInt(value)

  override def fromBigInt(bigInt: BigInt) = bigInt.toInt

  override def addH(a: Bits, b: Bits) = (a.asSInt + b.asSInt).resize(width).asBits

  override def subtractH(a: Bits, b: Bits) = (a.asSInt - b.asSInt).resize(width).asBits

  override def multiplyH(a: Bits, b: Bits) = (a.asSInt * b.asSInt).resize(width).asBits

  override def identityH(a: Bits) = a

  override def zeroH = B(0, width bits)

  override def oneH = B(1, width bits)

  override def undefined = Bits(width bits)

  override def toHard(coeff: Int) = B(coeff, width bits)


}

object IntField {
  def apply(width:Int): IntField = new IntField(width)
}
