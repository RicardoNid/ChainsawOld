package FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class DualClockBB extends BlackBox {
  val io = new Bundle {
    val a = in Vec(UInt(4 bits), 4)
    val b = in Vec(UInt(4 bits), 4)
    val c = out Vec(UInt(4 bits), 4)
  }
}

object DualClockBB {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new DualClockBB)
  }
}

object testDualClockBB {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new DualClockBB).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.risingEdge()
          while (true) {
            dut.clockDomain.clockToggle()
            sleep(1)
          }
        }
        val mainThread = fork {
          // test vectors
          simSuccess()
        }
      }
  }
}
