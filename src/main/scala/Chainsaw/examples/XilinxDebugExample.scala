package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real
import xilinx._

case class XilinxDebugExample() extends Component {
  val core    = CounterFreeRun(13)
  val dataOut = out(core.value)
  dataOut.addAttribute("mark_debug", "true")
  println(dataOut.getName())
}

object XilinxDebugExample {
  def main(args: Array[String]): Unit = {
    GenRTL(XilinxDebugExample())
    //        VivadoSynth(new XilinxDebugExample(), "debugCounter")
    //    VivadoImpl(new XilinxDebugExample(), "debugCounter")
    val dut = XilinxDebugExample()
    val builder = VivadoConstraintBuilder()
      .setLoc("A13", dut.dataOut.getName())
    println(builder.commands.mkString(" "))
  }
}
