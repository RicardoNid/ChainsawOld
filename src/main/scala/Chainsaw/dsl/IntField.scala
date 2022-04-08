package Chainsaw.dsl

class IntField extends Field[Int] {
  override def add(a: Int, b: Int) = a + b

  override def subtract(a: Int, b: Int) = a - b

  override def multiply(a: Int, b: Int) = a * b

  override def identity(a: Int) = a

  override def zero = 0
}

object IntField {
  def apply(): IntField = new IntField()
}
