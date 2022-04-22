package Chainsaw.dsl.field

import spinal.core._

import scala.reflect.ClassTag

abstract class Field[T] {

  val width: Int

  def toBits(value: T): String

  def fromBits(bits: String): T

  def toBigInt(value: T) = BigInt(toBits(value), 2)

  def fromBigInt(bigInt: BigInt) = fromBits(bigInt.toString(2))

  def undefined = Bits(width bits)

  def add(a: T, b: T): T

  def subtract(a: T, b: T): T

  def multiply(a: T, b: T): T

  def identity(a: T): T

  def zero: T

  def one: T

  def random: T

  def addH(a: Bits, b: Bits): Bits

  def subtractH(a: Bits, b: Bits): Bits

  def multiplyH(a: Bits, b: Bits): Bits

  def identityH(a: Bits): Bits

  def zeroH: Bits

  def oneH: Bits

  def toHard(coeff: T): Bits
}
