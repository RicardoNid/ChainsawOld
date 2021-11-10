package Chainsaw.examples

import spinal.core._

/** this example shows that "Hardware evaluations" in SpinalHDL need a context
 *
 */
object ContextProblemExample {
  def main(args: Array[String]): Unit = {

    // without a context, you can't even instantiate a simple literal
    //    U(3, 2 bits)

    // an alternative is: you can build it as a function
    val add = (a: UInt, b: UInt) => a + b

    // and this function can even be used to build a larger one
    // by this way, you can build huge component without really generate/test anything
    val add2 = (a: UInt, b: UInt) => a + add(a, b)

    // however, once it is evaluated, that is, once anything <: Data is involved, it won't work
    //    add2(U(3, 2 bits), U(4, 3 bits))

    // to "run" this function, you need a hardware context
    new Component {
      val a = in UInt (2 bits)
      val b = in UInt (3 bits)
      val ret = out(add2(a, b))
    }

  }
}
