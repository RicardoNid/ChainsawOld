package examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.Random

class CounterWithClear(width: Int) extends Component {
  val io = new Bundle {
    val clear = in Bool
    val value = out UInt (width bits)
  }
  val register = Reg(UInt(width bits)) init (0)
  register := register + 1
  when(io.clear) {
    register := 0
  }
  io.value := register
}

object CounterWithClear {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new CounterWithClear(8))
  }
}

object testCounterWithClear {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new CounterWithClear((4))).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          // Clear the clock domains' signals, to be sure the simulation captures their first edges.
          dut.clockDomain.risingEdge()
          dut.clockDomain.deassertReset()
          while (true) {
            dut.clockDomain.clockToggle()
            sleep(1)
          }
        }

        val mainThread = fork {
          var idx = 0
          var count = 0
          while (idx < 100) {
            if (idx % 15 == 0) {
              dut.io.clear #= true
              count = 0
            } else {
              dut.io.clear #= false
              count += 1
            }
            sleep(2)
            assert(dut.io.value.toInt == count, "count = " + count + " yours = " + dut.io.value.toInt)
            idx += 1
          }
          simSuccess()
        }
      }
  }
}
