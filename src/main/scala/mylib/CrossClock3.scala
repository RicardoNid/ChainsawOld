package mylib

import spinal.core._
import spinal.lib._

class CrossClock3 extends Component {

  // Use the current clock domain : 100MHz
  val areaStd = new Area {
    val counter = out(CounterFreeRun(16).value)
  }

  // Slow the current clockDomain by 4 : 25 MHz
  val areaDiv4 = new SlowArea(4) {
    val counter = out(CounterFreeRun(16).value)
  }

  // Slow the current clockDomainn to 50MHz
  val area50Mhz = new SlowArea(50 MHz) {
    val counter = out(CounterFreeRun(16).value)
  }
}

object CrossClock3 {
  def main(args: Array[String]) {
    new SpinalConfig(
      defaultClockDomainFrequency = FixedFrequency(100 MHz)
    ).generateVerilog(new CrossClock3)
  }
}

