package tutorial.examples.AXI

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._

class AXIInterconnet extends Component {
  val io = new Bundle {
    Axi4Ar
  }
}


object AXIInterconnet {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new AXIInterconnet)
  }
}

object testAXIInterconnet {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new AXIInterconnet).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          simSuccess()
        }
      }
  }
}
