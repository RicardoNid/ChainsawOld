package Chainsaw.examples

import spire.algebra._ // provides algebraic type classes
import spire.math._ // provides functions, types, and type classes
import spire.implicits._ // provides infix operators, instances and conversions

object SpireExample {
  def main(args: Array[String]): Unit = {
    val n1 = r"1/3"
    val n2 = r"1599/115866" // simplified at compile-time to 13/942
    println(n1 + n2)
    println(n1 * n2)

  }
}

