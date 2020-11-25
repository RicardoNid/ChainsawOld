// design : 使用BlackBox处理时钟IP,解耦时钟逻辑和业务逻辑

package examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random

// generated IP
// module singleClock
// (
//  // Clock out ports
//  output        clk_out1,
//  // Status and control signals
//  input         reset,
//  output        locked,
// // Clock in ports
//  input         clk_in1_p,
//  input         clk_in1_n
// );

class singleClock extends BlackBox {
  val io = new Bundle {
    val clk_in1_p = in Bool
    val clk_in1_n = in Bool

    val clk_out1 = out Bool
    val locked = out Bool
  }

  addRTLPath("e:/LtrProjects/VivadoProjects/ZCU_efficient/ZCU_efficient.srcs/sources_1/ip/singleClock/singleClock.v")

  noIoPrefix()
}

class PLLWraper extends Component {
  val io = new Bundle {
    val aReset = in Bool
    val clk_in1_p = in Bool
    val clk_in1_n = in Bool
    val result = out UInt (4 bits)
  }

  val clkCtrl = new Area { // design : 在专门的Area中处理所有clock/reset事宜
    val pll = new singleClock
    pll.io.clk_in1_p := io.clk_in1_p
    pll.io.clk_in1_n := io.clk_in1_n

    val coreClockDomain = ClockDomain.internal(
      name = "core",
      frequency = FixedFrequency(200 MHz)
    )

    coreClockDomain.clock := pll.io.clk_out1
    coreClockDomain.reset := ResetCtrl.asyncAssertSyncDeassert(
      input = io.aReset || !pll.io.locked,
      clockDomain = coreClockDomain
    )
  }

  val core = new ClockingArea(clkCtrl.coreClockDomain) { // design : 业务逻辑
    val counter = Reg(UInt(4 bits)) init(0)
    counter := counter + 1
    io.result := counter
  }
}

object PLLWraper {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new PLLWraper)
  }
}


