package FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class adder extends Component {
  val io = new Bundle {
    val a = in Vec(UInt(4 bits), 4)
    val b = in Vec(UInt(4 bits), 4)
    val c = out Vec(UInt(4 bits), 4)
  }

  val c_next = Reg(Vec(UInt(4 bits), 4))

  for (i <- 0 until 4) {
    c_next(i) := io.a(i) + io.b(i)
  }

  io.c := c_next

}

object adder {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new adder)
  }
}

object testadder {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new adder).
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
