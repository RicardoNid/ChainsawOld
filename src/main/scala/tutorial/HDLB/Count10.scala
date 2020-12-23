package tutorial.HDLB

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Count10
class Count10 extends Component {

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val q = out UInt(4 bits)
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.reset,
    config = new ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    val counter = Counter(0 until 10, True)
    io.q := counter
  }

  noIoPrefix()
}

object Count10 {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "Count10.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new Count10().setDefinitionName("top_module"))
  }
}
