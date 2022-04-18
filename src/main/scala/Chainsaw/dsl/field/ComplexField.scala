package Chainsaw.dsl.field

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

class ComplexField extends Field[Complex] {
  override def add(a: Complex, b: Complex) = a + b

  override def subtract(a: Complex, b: Complex) = a - b

  override def multiply(a: Complex, b: Complex) = a * b

  override def identity(a: Complex) = a

  override def zero = Complex(0, 0)

  override def one = Complex(1,0)
}

object ComplexField {
  def apply(): ComplexField = new ComplexField()
}


