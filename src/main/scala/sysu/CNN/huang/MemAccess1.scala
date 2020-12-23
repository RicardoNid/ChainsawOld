package sysu.CNN.huang

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._

import spinal.core.sim._
import scala.util.Random

import sysu.xilinx._
import sysu.util._

class MemAccess1 extends Component {
  val io = new Bundle {

    // fixme : 占位
    val inputWriteSel = in UInt (4 bits)
    val weightWriteSel = in UInt (6 bits)
    val inputDataIn = in UInt (8 bits)
    val weightDataIn = in UInt (8 bits)
    val inputAddr = in UInt (log2Up(4096) bits)
    val weightAddr = in UInt (log2Up(16) bits)

    val inputDataOut = out Vec(UInt(8 bits), 16)
    val weightDataOut = out Vec(Vec(UInt(8 bits), 16), 16)
  }

  val inputRAMs = (0 until 16).map(i => Mem(UInt(8 bits), 4096)) // 16 * BRAM18
  val weightRAMs = (0 until 64).map(i => Mem(UInt(32 bits), 16)) // 64 * BRAM18

  val weightAddrSeqGen = Counter(16, True)
  val weightAddrSeqGenPeriodic = Counter(32 * 32, weightAddrSeqGen.willOverflow)

  val inputAddrSeqGenInner = Counter(16, True) // fixme
  val inputAddrSeqGenPeriodic = Counter(64, True)
  val inputAddrSeqGenOuter = Counter(256, inputAddrSeqGenPeriodic.willOverflow)

  val inputAddr = inputAddrSeqGenOuter @@ inputAddrSeqGenInner
  val weightAddr = weightAddrSeqGen

  val inputDone = inputAddrSeqGenOuter.willOverflow
  val weightDone = weightAddrSeqGenPeriodic.willOverflow

  // 读端口 : 按地址序列读BRAM
  (0 until 16).foreach(i => io.inputDataOut(i) := inputRAMs(i).readSync(inputAddr))

  for (n <- 0 until 16; c <- 0 until 16) {
    val port = n % 16 * 4 + c / 4 % 4
    val byte = c % 4
    io.weightDataOut(n)(c) := weightRAMs(port).readSync(weightAddr)(byte * 8 until (byte + 1) * 8)
  }

  // 写端口
  val inputWe = Vec(Bool, 16)
  val weightWe = Vec(Bool, 64)
  (0 until 16).foreach(i => inputWe(i) := (io.inputWriteSel === U(i)))
  (0 until 64).foreach(i => weightWe(i) := (io.weightWriteSel === U(i)))

  (0 until 16).foreach(i => inputRAMs(i).write(io.inputAddr, io.inputDataIn, inputWe(i)))

  (0 until 64).foreach(i => weightRAMs(i).write(io.weightAddr, io.weightDataIn, weightWe(i)))

  noIoPrefix()
}

object MemAccess1 {
  def main(args: Array[String]): Unit = {
    val moduleName = "MemAccess1"
    val report = VivadoFlow(
      design = new MemAccess1(),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/util/MemAccess1"),
      force = true).doit()
    report.printArea
    report.printFMax
  }
}

object testMemAccess1 {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new MemAccess1).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          sleep(20000 * period)
          simSuccess()
        }
      }
  }
}