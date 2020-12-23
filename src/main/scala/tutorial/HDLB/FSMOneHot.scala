package tutorial.HDLB

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Fsm_onehot
class FSMOneHot extends Component { // todo : undone

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val input = in Bool
    val output = out Bool
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = new ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    io.output := io.input
  }

  noIoPrefix()
}

object FSMOneHot {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "FSMOneHot.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new FSMOneHot().setDefinitionName("top_module"))
  }
}
