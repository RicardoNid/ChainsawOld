package sysu.CNN.huang

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.bram._
import spinal.core.sim._
import scala.util.Random

import sysu.xilinx._
import sysu.util._

class Write1 extends Component {
  val io = new Bundle {
    val calc2write_1 = slave(Flow(Vec(word_t, 16)))
    val write2mem_1 = master(BRAM(BRAMConfig(128, log2Up(128))))
    val write2memCtrl_1 = master(Stream(Bits(0 bits)))
    val writeDone = out Bool
    val warning = out Bool
  }

  val mapCount = Counter(0 until 4096, io.calc2write_1.valid)
  val rowCount = Counter(0 until 128, io.calc2write_1.valid) // 实际上

  io.writeDone := rowCount.willOverflow

  val memPort = io.write2mem_1
  val memCtrl = io.write2memCtrl_1

  memCtrl.valid := io.calc2write_1.valid

  memPort.en := memCtrl.ready
  memPort.we.setAll()
  memPort.addr := rowCount
  memPort.wrdata := io.calc2write_1.payload.asBits

  io.warning := io.calc2write_1.valid && !memCtrl.ready
}

object Write1 {
  def main(args: Array[String]): Unit = {
    val moduleName = "Write1"
    val report = VivadoFlow(
      design = new Write1(),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/huang/Write1"),
      force = true).doit()
    report.printArea
    report.printFMax
  }
}

object testWrite1 {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Write1).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          dut.io.write2memCtrl_1.ready #= true
          dut.io.calc2write_1.valid #= true
          dut.io.calc2write_1.payload.foreach(_ #= 13)
          sleep(1000 * period)
          simSuccess()
        }
      }
  }
}
