package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class UndeterminedWidth() extends Component {

  val input  = in Vec (UInt(4 bits), 2)
  val output = out UInt ()

  output := input(0) + input(1)

}

object UndeterminedWidth {
  def main(args: Array[String]): Unit = {
    GenRTL(UndeterminedWidth())
  }
}
