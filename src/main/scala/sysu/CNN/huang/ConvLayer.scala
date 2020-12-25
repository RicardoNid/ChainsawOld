package sysu.CNN.huang

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import sysu.CNN._

import scala.util.Random
import sysu.xilinx._
import sysu.util._

class ConvLayer(loopNestConv: LoopNestConv, next: LoopNestConv) extends Component {

  import loopNestConv._

  val io = new Bundle {
    val inputDataIn = slave(Stream(Vec(word_t, Pif)))
    val weightDataIn = slave(Stream(Vec(Vec(word_t, Pif), Pof)))

    val inputDataIn_2 = master(Stream(Vec(word_t, next.Pif)))
    val weightDataIn_2 = master(Stream(Vec(Vec(word_t, next.Pif), next.Pof)))
  }

  val calc1 = new Calc(loopNestConv)
  val write1 = new Write(loopNestConv)
  val mem1 = new Buffer(loopNestConv, next)
  val read1 = new Read(loopNestConv, next)

  calc1.io.inputDataIn <>io.inputDataIn
  calc1.io.weightDataIn <> io.weightDataIn
  calc1.io.calc2write_1 <> write1.io.calc2write_1

  write1.io.write2memCtrl_1 <> mem1.io.write2memCtrl_1
  write1.io.write2mem_1 <> mem1.io.write2mem_1
  mem1.io.writeDone := write1.io.writeDone

  read1.io.mem2readCtrl_1 <> mem1.io.mem2readCtrl_1
  read1.io.mem2read_1 <> mem1.io.mem2read_1
  mem1.io.readDone := read1.io.readDone
  mem1.io.readSel := read1.io.readSel

  io.inputDataIn_2 <> read1.io.inputDataIn_2
  io.weightDataIn_2 <> read1.io.weightDataIn_2
}

object ConvLayer {
  def main(args: Array[String]): Unit = {
    val moduleName = "ConvLayer"
    val report = VivadoFlow(
      design = new ConvLayer(loopNestHuang1, loopNestHuang2),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/huang/ConvLayer"),
      force = true).doit()
    report.printArea
    report.printFMax
  }
}

object testConvLayer {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new ConvLayer(loopNestHuang1, loopNestHuang2)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          dut.io.inputDataIn.valid #= true
          dut.io.weightDataIn.valid #= true
          dut.io.inputDataIn_2.ready #= true
          dut.io.weightDataIn_2.ready #= true
          sleep(1000000 * period)
          simSuccess()
        }
      }
  }
}