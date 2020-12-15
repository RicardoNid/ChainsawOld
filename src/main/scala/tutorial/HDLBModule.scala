package tutorial

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class HDLBModule extends Component {

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

  val mainCD = new ClockingArea(CD){
      io.output := io.input
  }

  noIoPrefix()
}

object HDLBModule {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "HDLBModule.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new HDLBModule().setDefinitionName("top_module"))
  }
}
