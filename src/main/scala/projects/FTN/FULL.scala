package projects.FTN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import sysu.xilinx.VivadoFlowOld

import scala.util.Random

class FULL extends Component {
  val io = new Bundle {
    val a = in Vec(UInt(4 bits), 4)
    val b = in Vec(UInt(4 bits), 4)
    val c = out Vec(UInt(4 bits), 4)
  }
  (0 to 3).foreach(i => io.c(i) := io.a(i) & io.b(i))
}

object FULL {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = "output/FTN").generate(new FULL)
    val report = VivadoFlowOld(
      // todo : vivado flow infos
      workspacePath = "output/VIVADOFTN",
      toplevelPath = "output/FTN/FULL.sv"
    )
    println(report.getArea)
    println(report.getFMax() / 1E6 + "MHz")
  }
}

object testFULL {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new FULL).
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
