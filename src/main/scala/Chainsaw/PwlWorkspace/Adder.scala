package Chainsaw.PwlWorkspace

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class Adder() extends Component {
  val a, b = in UInt (8 bits)
  val c = out UInt (8 bits)

  c := a + b
}

object Adder extends App {

}
