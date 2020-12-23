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

class Mem1 extends Component {
  val io = new Bundle {

    val write2mem_1 = slave(BRAM(BRAMConfig(128, log2Up(128)))) // design : !!
    val write2memCtrl_1 = slave(Stream(Bits(0 bits)))
    val writeDone = in Bool

    //    val mem2read_1 = master(BRAM(BRAMConfig(128, log2Up(128)))) // todo : 设计解决K+S访存的方法,(建立value -> addr映射)
    val mem2readCtrl_1 = master(Stream(Bits(0 bits)))
    val readDone = in Bool
  }

  val RAMs = (0 until 4).map(i => Mem(Bits(128 bits), 128))
  val writeMask = Reg(Bits(4 bits)) init (B(8)) // 1000

  when(io.writeDone)(writeMask.rotateRight(1))
  // 写端口
  (0 until 4).foreach(i => RAMs(i).write(
    address = io.write2mem_1.addr,
    data = io.write2mem_1.wrdata,
    enable = writeMask(i) && io.write2memCtrl_1.fire
  ))

  io.write2mem_1.rddata := B(0) // 之后要丰富BRAM接口,派生出读写种类

  //  val readSwitch = Reg(Vec(Bits(4 bits), 3)) init (Vec(B(8, 4 bits), B(4, 4 bits), B(2, 4 bits))) // 1000 0100 0010
  // 读端口

  val fsm = new StateMachine {
    val EMPTY = stateBoot
    setEntry(EMPTY)
    disableAutoStart()
    val ONE = State()
    val TWO = State()
    val THREE = State()
    val FULL = State()

    EMPTY
      .whenIsActive(when(io.writeDone)(goto(ONE)))

    ONE
      .whenIsActive {
        when(io.writeDone && io.readDone)(goto(ONE))
          .elsewhen(io.writeDone(goto(TWO)))
          .elsewhen(io.readDone(goto(EMPTY)))
      }

    TWO
      .whenIsActive {
        when(io.writeDone && io.readDone)(goto(TWO))
          .elsewhen(io.writeDone(goto(THREE)))
          .elsewhen(io.readDone(goto(ONE)))
      }

    THREE
      .whenIsActive {
        when(io.writeDone && io.readDone)(goto(THREE))
          .elsewhen(io.writeDone(goto(TWO)))
          .elsewhen(io.readDone(goto(FULL)))
      }

    FULL
      .whenIsActive {
        when(io.readDone)(goto(THREE))
      }

    io.write2memCtrl_1.ready := isActive(EMPTY) || isActive(ONE) || isActive(TWO) || isActive(THREE)
    io.mem2readCtrl_1.valid := isActive(THREE) || isActive(FULL)
  }
}

object Mem1 {
  def main(args: Array[String]): Unit = {
    val moduleName = "Mem1"
    val report = VivadoFlow(
      design = new Mem1(),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/huang/Mem1"),
      force = true).doit()
    report.printArea
    report.printFMax
  }
}

object testMem1 {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Mem1).
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
