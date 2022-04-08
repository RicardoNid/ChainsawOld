package Chainsaw.dsl

import spinal.core._

import scala.language.postfixOps

class IntSignalField extends Field[SInt] with HasConstant[Int, SInt] {
  override def add(a: SInt, b: SInt) = a +^ b

  override def subtract(a: SInt, b: SInt) = a -^ b

  override def multiply(a: SInt, b: SInt) = a * b

  override def identity(a: SInt) = a

  override def zero = S(0)

  override def fromConstant(constant: Int) = S(constant)
}

object IntSignalField {
  def apply(): IntSignalField = new IntSignalField()
}
