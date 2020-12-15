package FTN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

class ClockCross(bitWidth: Int, ratio: Int) extends Component {
  val io = new Bundle {
    val clkLow = in Bool
    val rstLow = in Bool
    val lowFreqInput = in Vec(Bits(bitWidth bit), ratio)
    val clkHigh = in Bool
    val rstHigh = in Bool
    val HighFreqOutput = out Bits (bitWidth bit)
  }

  val areaLow = new ClockingArea(new ClockDomain(clock = io.clkLow, reset = io.rstLow, frequency = FixedFrequency(500 MHz))) {
    val reg = RegNext(io.lowFreqInput)
  }
  val areaHigh = new ClockingArea(new ClockDomain(clock = io.clkHigh, reset = io.rstHigh, frequency = FixedFrequency(500 MHz))) {

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

object testClockCross {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(
      new ClockCross(8, 5) {
      }
    ).doSimUntilVoid { dut =>
      val clockThread = fork {
        dut.areaLow.clockDomain.forkStimulus(period = 5 * period)
        dut.areaHigh.clockDomain.forkStimulus(period)
      }
      (0 until 5).foreach(i => dut.io.lowFreqInput(i) #= i)
      sleep(1000 * period)
      simSuccess()
    }
  }
}

