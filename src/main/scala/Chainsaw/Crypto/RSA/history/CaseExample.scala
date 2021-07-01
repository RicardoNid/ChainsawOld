package Chainsaw.Crypto.RSA.history

import Chainsaw._
import spinal.core._

class CaseExample extends Component {
  val input = in UInt (4 bits)
  val output = out UInt (4 bits)
  val condition = in Bits(2 bits) // one-hot

  switch(True) {
    is(condition(0))(output := input + 0)
    is(condition(1))(output := input + 1)
    default(output := input + 2)
  }
}

object CaseExample {
  def main(args: Array[String]): Unit = {
    GenRTL(new CaseExample)
  }
}
