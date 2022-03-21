package Chainsaw.examples

import breeze.linalg._
import breeze.math.Semiring
import spinal.core._

import scala.reflect.ClassTag

class UIntRing(width: Int) extends Semiring[UInt] {

  override def zero = U(0, width bits)

  override def one = U(1, width bits)

  override def +(a: UInt, b: UInt) = a + b

  override def *(a: UInt, b: UInt) = (a * b).resize(width)

  override def ==(a: UInt, b: UInt) = true

  override def !=(a: UInt, b: UInt) = true
}
