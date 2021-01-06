package projects.huang

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._
import spinal.lib.bus.bram._
import sysu.CNN._

import scala.util.Random
import sysu.xilinx._
import sysu.util._

class Read(loopNestConv: LoopNestConv, next: LoopNestConv) extends Component {

  import loopNestConv._

  val io = new Bundle {
    val mem2read_1 = master(BRAM(BRAMConfig(next.Pif * dataWidth, log2Up(next.inputRowSize / next.Pif)))) // todo : 设计解决K+S访存的方法,(建立value -> addr映射)
    val mem2readCtrl_1 = slave(Stream(Bits(0 bits)))
    val readDone = out Bool
    val readSel = out Bits (next.Nkx bits) // 暂定方法是由reader给出one-hot RAM选择信号

    val inputDataIn_2 = master(Stream(Vec(word_t, Pif)))
    val weightDataIn_2 = master(Stream(Vec(Vec(word_t, next.Pif), next.Pof)))
  }

  // control by counter, SRL and handshake


  val selReg = RegInit(B"100")
  selReg := selReg.rotateRight(1)

  io.inputDataIn_2.valid := io.mem2readCtrl_1.valid
  io.weightDataIn_2.valid := io.mem2readCtrl_1.valid
  io.mem2readCtrl_1.ready := io.inputDataIn_2.ready && io.weightDataIn_2.ready // todo : 实现反压

  // todo 实现
  // addrGen i % 3 * 4 + i % 9 / 3 * 128 + i % 36 / 9 + i / 72 * 4 + (-132)
  val addrCounter0 = Counter(3, io.mem2readCtrl_1.fire)
  val addrCounter1 = Counter(3, addrCounter0.willOverflow)
  val addrCounter2 = Counter(4, addrCounter1.willOverflow)
  val addrCounter3 = Counter(2, addrCounter2.willOverflow)
  val rowCounter = Counter(0 until next.Nox, addrCounter3.willOverflow)
  val mapCounter = Counter(0 until next.Noy, rowCounter.willOverflow)

  // datapather
  io.readDone := rowCounter.willOverflow
  io.readSel := selReg

  io.mem2read_1.en := io.mem2readCtrl_1.fire
  io.mem2read_1.we.setAllTo(False)
  io.mem2read_1.addr := U(0) // todo : 实现地址发生
  (0 until next.Pif).foreach(i => io.inputDataIn_2.payload(i) := io.mem2read_1.rddata(i * 8 until (i + 1) * 8).asUInt)

  io.mem2read_1.wrdata.setAllTo(False) // todo : 避免这项冗余
  for (n <- 0 until next.Pof; c <- 0 until next.Pif) io.weightDataIn_2.payload(n)(c) := U(0) // todo : 避免这项冗余
}

object Read {
  def main(args: Array[String]): Unit = {
    val moduleName = "Read1"
    val report = VivadoFlow(
      design = new Read(loopNestHuang1, loopNestHuang2),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/huang/Read1")).doit()
    report.printArea
    report.printFMax
  }
}

object testRead {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Read(loopNestHuang1, loopNestHuang2)).
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
