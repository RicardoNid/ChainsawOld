package Chainsaw.dsl.field

class RealField extends Field[Double] {

  override def add(a: Double, b: Double) = a + b

  override def subtract(a: Double, b: Double) = a - b

  override def multiply(a: Double, b: Double) = a * b

  override def identity(a: Double) = a

  override def zero = 0.0

  override def one = 1.0
}

object RealField {
  def apply(): RealField = new RealField()
}
