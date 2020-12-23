package tutorial.HDLB.fancytimer

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Exams/review2015_fsmshift
class ShiftFSM extends Component {

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val shift_ena = out Bool
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = new ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    val timeout = Timeout(4)
    io.shift_ena := Mux(timeout, False, True)
  }

  noIoPrefix()
}

object ShiftFSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "ShiftFSM.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new ShiftFSM().setDefinitionName("top_module"))
  }
}
