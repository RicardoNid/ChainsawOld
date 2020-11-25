package DSP

import spinal.core._
import spinal.lib._

class ClockCross(bitWidth: Int, ratio: Int) extends Component {
  val io = new Bundle {
    val clkLow = in Bool
    val rstLow = in Bool
    val lowFreqInput = in Vec(Bits(bitWidth bit), ratio)
    val clkHigh = in Bool
    val rstHigh = in Bool
    val HighFreqOutput = out Bits (bitWidth bit)
  }

  val areaLow = new ClockingArea(new ClockDomain(clock = io.clkLow, reset = io.rstLow)) {
    val reg = RegNext(io.lowFreqInput)
  }
  val areaHigh = new ClockingArea(new ClockDomain(clock = io.clkHigh, reset = io.rstHigh)) {
    val selCounter = Counter(0 until ratio, True)
    val sel = selCounter.value
    val buf = BufferCC(areaLow.reg(sel))
  }

  io.HighFreqOutput := areaHigh.buf
}

object ClockCross {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new ClockCross(8, 4))
  }
}
