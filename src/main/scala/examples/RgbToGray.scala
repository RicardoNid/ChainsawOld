package examples

import spinal.core._
import spinal.lib._
import spinal.core.sim._

class RgbToGray extends Component {
  val io = new Bundle {
    val clear = in Bool
    val r, g, b = in UInt (8 bits) // design : 可以一次性声明多个同类型信号
    val wr = out Bool
    val address = out UInt (16 bits)
    val data = out UInt (8 bits)
  }

  def coef(value: UInt, by: Float): UInt = (value * U((255 * by).toInt, 8 bits) >> 8)

  val gray = RegNext(
    coef(io.r, 0.4f) + // design : f后缀,float
      coef(io.g, 0.4f) +
      coef(io.b, 0.3f)
  )

  val address = CounterFreeRun(1 << 16) // design : CounterFreeRun在给定范围内循环
  io.address := address
  io.wr := True
  io.data := gray

  when(io.clear) {
    gray := 0
    address.clear()
    io.wr := False
  }
}

object RgbToGray {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new RgbToGray)
  }
}

object testRgbToGray {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new RgbToGray).
      doSim { dut =>
        val clockThread = fork {
          dut.clockDomain.risingEdge()
          while (true) {
            dut.clockDomain.clockToggle()
          }
        }

        val mainThread = fork {
          dut.io.r #= 100
          dut.io.g #= 50
          dut.io.b #= 35
          sleep(2)
          assert(dut.io.data.toInt == (100 * 0.4 + 50 * 0.4 + 35 * 0.3).toInt)
        }
        simSuccess()
      }
  }
}
