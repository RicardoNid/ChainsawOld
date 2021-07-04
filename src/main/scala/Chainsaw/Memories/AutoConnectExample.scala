package Chainsaw.Memories

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class Port1(hasC: Boolean = true) extends Bundle {
  val a = Bool()
  val b = Bool()
  val c = if (hasC) Bool() else null
}

case class Port2() extends Bundle {
  val a = Bool()
  val b = Bool()
}

class AutoConnectExample extends Component {
  val input = in(Port1(false))
  val output = out(Port1())
}

object AutoConnectExample {
  def main(args: Array[String]): Unit = {
    GenRTL(new AutoConnectExample)
  }
}


