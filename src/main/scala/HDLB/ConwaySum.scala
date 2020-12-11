package HDLB

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class ConwaySum extends Component {
  val io = new Bundle {
    val center = in Bool
    val neighbors = in Bits (9 bits)
    val centerOut = out Bool
  }

  var sum = U"0000"
  for(i <- 0 until 9 if (i != 4)) sum \= sum + io.neighbors(i).asUInt
  when(sum < U(2) || sum > U(3))(io.centerOut := False)
    .elsewhen(sum === U(3))(io.centerOut := True)
    .otherwise(io.centerOut := io.center)
}

object ConwaySum {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new ConwaySum)
  }
}

object testConwaySum {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new ConwaySum).
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
