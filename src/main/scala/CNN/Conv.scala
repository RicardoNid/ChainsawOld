package CNN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class Conv extends Component {
  val io = new Bundle {
    val a = in Vec(UInt(4 bits), 4)
    val b = in Vec(UInt(4 bits), 4)
    val c = out Vec(UInt(4 bits), 4)
  }
}

object Conv {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Conv)
  }
}

object testConv {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Conv).
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
