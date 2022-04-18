package Chainsaw.dsl.field

import Chainsaw.dsl.HardField
import spinal.core._

import scala.language.postfixOps

class SIntField extends Field[SInt] with HardField[Int, SInt] {

  override def add(a: SInt, b: SInt) = a +^ b

  override def subtract(a: SInt, b: SInt) = a -^ b

  override def multiply(a: SInt, b: SInt) = a * b

  override def identity(a: SInt) = a

  override def zero = S(0)

  override def one = S(1)

  override def fromConstant(constant: Int) = S(constant)

  override def undefined = SInt(8 bits)

}

object SIntField {
  def apply(): SIntField = new SIntField()
}
