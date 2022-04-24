package Chainsaw.dsl.field

import spinal.core._

import scala.reflect.{ClassTag, classTag}

abstract class MixType[T:ClassTag] {

  val tag = classTag[T]

  val width: Int

  def toBits(value: T): String

  def fromBits(bits: String): T

  def toBigInt(value: T) = BigInt(toBits(value), 2)

  def fromBigInt(bigInt: BigInt) = fromBits(bigInt.toString(2))

  def undefined = Bits(width bits)

  def toCoeff(coeff: T) = B(toBigInt(coeff), width bits)
}
