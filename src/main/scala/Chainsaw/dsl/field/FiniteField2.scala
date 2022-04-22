package Chainsaw.dsl.field

import spinal.core.{Bits, _}

import scala.util.Random
import Chainsaw.dsl._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class FiniteField2(modulo: FiniteInt) extends Field[FiniteInt] {

  require(isPow2(modulo.value))

  override val width = log2Up(modulo.value)

  override def toBits(value: FiniteInt) = value.value.toBinaryString.padToLeft(width, '0')

  override def fromBits(bits: String) = FiniteInt(BigInt(bits, 2).toInt)

  override def add(a: FiniteInt, b: FiniteInt) = (a.value + b.value) % modulo.value

  override def subtract(a: FiniteInt, b: FiniteInt) = (a.value - b.value) % modulo.value

  override def multiply(a: FiniteInt, b: FiniteInt) = (a.value * b.value) % modulo.value

  override def identity(a: FiniteInt) = a

  override def zero = 0

  override def one = 1

  override def random = Random.nextInt(modulo.value)

  override def addH(a: Bits, b: Bits) = (a.asUInt + b.asUInt).asBits

  override def subtractH(a: Bits, b: Bits) = (a.asUInt - b.asUInt).asBits

  override def multiplyH(a: Bits, b: Bits) = (a.asUInt * b.asUInt).asBits

  override def identityH(a: Bits) = a

  override def zeroH = B(0, width bits)

  override def oneH = B(1, width bits)

  override def undefined = Bits(width bits)

  override def toHard(coeff: FiniteInt) = B(coeff.value, width bits)


}
