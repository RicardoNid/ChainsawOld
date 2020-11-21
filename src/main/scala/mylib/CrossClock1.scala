package mylib

import spinal.core._
import spinal.lib._
class CrossClock1 extends Component {
  val io = new Bundle {
    val clkA = in Bool
    val rstA = in Bool

    val clkB = in Bool
    val rstB = in Bool

    val dataIn  = in Bool
    val dataOut = out Bool
  }

  // sample dataIn with clkA
  val area_clkA = new ClockingArea(ClockDomain(io.clkA,io.rstA,
    frequency = FixedFrequency(100 MHz))) {
    val reg = RegNext(io.dataIn) init(False)

    println(ClockDomain.current.frequency)
  }

  // 2 register stages to avoid metastability issues
  val area_clkB = new ClockingArea(ClockDomain(io.clkB,io.rstB)) {
    val buf0   = RegNext(area_clkA.reg) init(False) addTag(crossClockDomain)
    val buf1   = RegNext(buf0)          init(False)
  }

  io.dataOut := area_clkB.buf1
}

object CrossClock1 {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new CrossClock1)
  }
}