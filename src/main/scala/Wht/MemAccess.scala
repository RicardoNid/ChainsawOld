package Wht

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import sysu.util._
import sysu.xilinx._


class MemAccess extends Component {

  val io = new Bundle {
    val dataIn = in UInt (10 bits)
    val dataOut = out Vec(UInt(10 bits), 3)
  }

  val delayLine = History(io.dataIn, 33, init = U(0, 10 bits))
  val positions = (0 until 3).map(i => SeqGen(seqs(i), LUT))
  (0 until 3).foreach(i => io.dataOut(i) := Mux(positions(i).resize(6) === U(0), U(0), delayLine(positions(i).resize(6) + 1)))
}

object MemAccess {
  def main(args: Array[String]): Unit = {
    val report = VivadoFlow(
      design = new MemAccess,
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = "MemAccess",
        workspacePath = "output/MemAccess"),
      force = true
    ).doit()
    report.printArea
    report.printFMax
  }
}

object testMemAccess {
  def main(args: Array[String]): Unit = {
    val period = 2
    SimConfig.withWave.compile(new MemAccess()).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        sleep(16 * period)
        for (i <- 0 until 512) {
          dut.io.dataIn #= reorder(i)
          sleep(period)
        }
        simSuccess()
      }
  }
}
