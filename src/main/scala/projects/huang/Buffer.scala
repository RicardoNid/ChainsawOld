package projects.huang

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bram._
import spinal.lib.fsm._
import sysu.CNN._
import sysu.util._
import sysu.xilinx._

class Buffer(loopNestConv: LoopNestConv, next: LoopNestConv) extends Component {

  import loopNestConv._

  val io = new Bundle {

    // design : loopNest中的简单row buffer存储映射 -> 端口宽度取决于并行度,深度取决于行数据数量, layout取决于展开方式
    // todo : 之后还要考虑进行切片的状况,考虑原本写的代码中哪些N其实是T,T其实是N
    val write2mem_1 = slave(BRAM(BRAMConfig(Pif * dataWidth, log2Up(inputRowSize / Pif))))
    val write2memCtrl_1 = slave(Stream(Bits(0 bits)))
    val writeDone = in Bool

    val mem2read_1 = slave(BRAM(BRAMConfig(next.Pif * dataWidth, log2Up(next.inputRowSize / next.Pif)))) // todo : 设计解决K+S访存的方法,(建立value -> addr映射)
    val mem2readCtrl_1 = master(Stream(Bits(0 bits)))
    val readDone = in Bool
    val readSel = in Bits (next.Nkx bits) // 暂定方法是由reader给出one-hot RAM选择信号
  }

  val numRAMs = next.Nkx + next.Stride
  val RAMs = (0 until numRAMs).map(i => Mem(Bits(128 bits), 128))
  // 写切换
  val writeMask = Reg(Bits(numRAMs bits)) init (B"1000") // 1000
  when(io.writeDone)(writeMask := writeMask.rotateRight(1))
  // 写端口
  (0 until numRAMs).foreach(i => RAMs(i).write(
    address = io.write2mem_1.addr,
    data = io.write2mem_1.wrdata,
    enable = writeMask(i) && io.write2memCtrl_1.fire
  ))

  io.write2mem_1.rddata := B(0) // todo : 之后要丰富BRAM接口,派生出读写种类

  // 读切换
  val readSwitch = RegInit(Vec(B"1000", B"0100", B"0010"))
  when(io.readDone)(readSwitch.foreach(mask => mask := mask.rotateRight(1)))
  // 读端口
  val readPorts = Vec(Bits(128 bits), numRAMs)
  (0 until numRAMs).foreach(i => readPorts(i) := RAMs(i).readSync(
    address = io.mem2read_1.addr,
    enable = io.mem2readCtrl_1.fire
  ))
  // 读映射
  val validReadPorts = CrossBar(readPorts, readSwitch)
  io.mem2read_1.rddata := MuxOH(io.readSel, validReadPorts)

  // todo : 状态机参数化
  val fsm = new StateMachine {
    // setting states
    val memStates = (0 to numRAMs).map(i => if (i == 0) stateBoot.setName(s"full_${i}") else State().setName(s"full_${i}"))
    setEntry(memStates(0))
    disableAutoStart()
    // state transition
    memStates.zipWithIndex.foreach { case (state, i) => {
      if (i == 0) state.whenIsActive(when(io.writeDone)(goto(memStates(i + 1))))
      else if (i == numRAMs) state.whenIsActive(when(io.readDone)(goto(memStates(i - 1))))
      else {
        state
          .whenIsActive {
            when(io.writeDone && io.readDone)(goto(state))
              .elsewhen(io.writeDone(goto(memStates(i + 1))))
              .elsewhen(io.readDone(goto(memStates(i - 1))))
          }
      }
    }
    }
    io.write2memCtrl_1.ready := memStates.dropRight(1).map(isActive(_)).reduce(_ || _)
    io.mem2readCtrl_1.valid := memStates.drop(next.Nkx).map(isActive(_)).reduce(_ || _)
  }
}

object Buffer {
  def main(args: Array[String]): Unit = {
    val moduleName = "Buffer"
    val report = VivadoFlow(
      design = new Buffer(loopNestHuang1, loopNestHuang2),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/huang/Buffer"),
      force = true).doit()
    report.printArea
    report.printFMax
  }
}

object testBuffer {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Buffer(loopNestHuang1, loopNestHuang2)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          for (i <- 0 until 1000) {
            dut.io.writeDone #= (if (i % 5 == 0) true else false)
            dut.io.readDone #= (if (i % 5 == 0) true else false)
            dut.io.write2memCtrl_1.valid #= true
            dut.io.mem2readCtrl_1.ready #= true
            sleep(period)
          }
          simSuccess()
        }
      }
  }
}
