package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

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
