package HDLB

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// https://hdlbits.01xz.net/wiki/Count_clock
class CountClock extends Component {
  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val ena = in Bool
    val pm = out UInt (1 bits)
    val hh = out UInt (8 bits)
    val mm = out Bits (8 bits)
    val ss = out Bits (8 bits)
  }

  val clockConfig = ClockDomainConfig(resetKind = SYNC) // design : 设置同步/异步reset
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.reset, config = clockConfig)) {
    // design : 设计代码

    val counterSs0 = Counter(0 to 9, io.ena)
    val counterSs1 = Counter(0 to 5, counterSs0.willOverflow)
    val counterMm0 = Counter(0 to 9, counterSs1.willOverflow)
    val counterMm1 = Counter(0 to 5, counterMm0.willOverflow)
    val counterHh = Counter(0 to 11, counterMm1.willOverflow)
    val counterPm = Counter(0 to 1, counterHh.willOverflow)

    io.ss := counterSs1.value.resize(4) ## counterSs0.value.resize(4)
    io.mm := counterMm1.value.resize(4) ## counterMm0.value.resize(4)
    switch(counterHh.value){
      for(i <- 1 to 9) is(U(i))(io.hh := counterHh.resize(8))
      is(U(10))(io.hh := U"8'h10")
      is(U(11))(io.hh := U"8'h11")
      is(U(0))(io.hh := U"8'h12")
      default (io.hh := U"8'h12")
    }
    io.pm := counterPm.value
  }
  noIoPrefix()
}

object CountClock {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new CountClock().setDefinitionName("top_module"))
  }
}
