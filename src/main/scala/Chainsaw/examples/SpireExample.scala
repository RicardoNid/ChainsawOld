package Chainsaw.examples

import spire.algebra._ // provides algebraic type classes
import spire.math._ // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions

case class minSum(value: Int)

class minSumRing(upperBound: Int) extends Ring[minSum] {
  override def one = minSum(0)

  override def negate(x: minSum) = minSum(-x.value)

  override def times(x: minSum, y: minSum) = minSum(x.value + y.value)

  override def zero = minSum(upperBound)

  override def plus(x: minSum, y: minSum) = minSum(min(x.value, y.value))
}

object SpireExample {

  def main(args: Array[String]): Unit = {

    implicit val ring: minSumRing = new minSumRing(128)

  }

}
