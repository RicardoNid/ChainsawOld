package tutorial.HDLB

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Countbcd extends Component {
  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val ena = out Bits (3 bits)
    val q = out Bits (16 bits)
  }

  val clockConfig = ClockDomainConfig(resetKind = SYNC)
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.reset, config = clockConfig)) {
    // 更加熟悉集合类型之后,改写
    val counter1 = Counter(10, True)
    val counter2 = Counter(10, counter1.willOverflow)
    val counter3 = Counter(10, counter2.willOverflow)
    val counter4 = Counter(10, counter3.willOverflow)

    io.ena(2) := counter4.willIncrement
    io.ena(1) := counter3.willIncrement
    io.ena(0) := counter2.willIncrement
    io.q := counter4.value.asBits ## counter3.value.asBits ## counter2.value.asBits ## counter1.value.asBits
  }
  noIoPrefix()
}

object Countbcd {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Countbcd().setDefinitionName("top_module"))
  }
}
