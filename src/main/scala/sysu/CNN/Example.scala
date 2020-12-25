package sysu.CNN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import spinal.lib.bus.bram._

import scala.util.Random
import sysu.xilinx._
import sysu.util._

class Example extends Component {
  val io = new Bundle {
    val input = slave(Stream(BRAM(BRAMConfig(128, log2Up(128)))))
    val output = master(Stream(BRAM(BRAMConfig(128, log2Up(128)))))
  }

  io.output <> io.input

}

object Example {
  def main(args: Array[String]): Unit = {
    val moduleName = "Example"
    val report = VivadoFlow(
      design = new Example(),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/huang/Example")).doit()
    report.printArea
    report.printFMax
  }
}

object testExample {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Example).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          sleep(1000 * period)
          simSuccess()
        }
      }
  }
}
