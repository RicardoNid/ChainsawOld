package tutorial.basic

import spinal.core._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Exams/ece241_2014_q4
class DFFAndGates extends Component {

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

object DFFAndGates {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "DFFAndGates.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new DFFAndGates().setDefinitionName("top_module"))
  }
}