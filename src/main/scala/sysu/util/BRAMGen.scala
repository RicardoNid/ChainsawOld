package sysu.util

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.bram._
import spinal.core.sim._

import sysu.xilinx._
import sysu.util._

// 需要手动堆叠地址
class BRAMGen extends Component {
  val io = new Bundle {
    val input = in UInt (64 bits)
    val sel = in UInt (3 bits)
    val addr = in UInt (4 bits)
    val output = out UInt (64 bits)
  }
  val count8 = Counter(8, True)
  val count16 = Counter(16, count8.willOverflow)
  val mems = (0 until 8).map(_ => Mem(UInt(64 bits), 16))

  val candidate = Vec(UInt(64 bits), 8)
  (0 until 8).foreach(i => candidate(i) := mems(i).readSync(count16.value))

  for (i <- 0 until 8) {
    when(io.sel === U(i))(mems(i)(io.addr) := io.input)
  }

  io.output := candidate(count8)
}

object BRAMGen {
  def main(args: Array[String]): Unit = {
    val report = VivadoFlow(
      design = new BRAMGen,
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = "bram",
        workspacePath = "output/bram"),
      force = true
    ).doit()
    report.printFMax
    report.printArea
  }
}
