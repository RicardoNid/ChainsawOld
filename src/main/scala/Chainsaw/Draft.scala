package Chainsaw

import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import Chainsaw._
import Chainsaw.Real

object Draft {

  class subdivide extends Component{
    val input = in Bits(8 bits)
    val output = out (input.subdivideIn(4 bits))
  }

  def main(args: Array[String]): Unit = {
    GenRTL(new subdivide)
  }

}
