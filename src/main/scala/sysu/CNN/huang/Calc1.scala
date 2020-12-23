package sysu.CNN.huang

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import sysu.CNN.AdderTree
import sysu.xilinx._

class Calc1 extends Component {
  val io = new Bundle {
    val inputDataIn = in Vec(word_t, 16)
    val weightDataIn = in Vec(Vec(word_t, 16), 16)

    val calc2write_1 = master(Flow(Vec(word_t, 16)))
  }

  // datapath
  val multRegs = Vec(Vec(double_word_t, 16), 16).addAttribute("use_dsp = \"yes\"")
  for (n <- 0 until 16; c <- 0 until 16) {
    multRegs(n)(c) := io.inputDataIn(c) * io.weightDataIn(n)(c)
  }
  val adderTrees = (0 until 16).map(i => AdderTree(multRegs(i), pipelined = true))
  val accRegs = Reg(Vec(output_t, 16))

  // control
  val validCount = Counter(4, True)
  when(validCount.willOverflow)((0 until 16).foreach(i => accRegs(i) := U(0)))
    .otherwise((0 until 16).foreach(i => accRegs(i) := accRegs(i) + adderTrees(i)))

  io.calc2write_1.valid := validCount.willOverflow
  io.calc2write_1.payload := accRegs.resized // todo : 更加精细地处理截断问题
}

object Calc1 {
  def main(args: Array[String]): Unit = {
    val moduleName = "Calc1"
    val report = VivadoFlow(
      design = new Calc1(),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(
        topModuleName = moduleName,
        workspacePath = s"output/huang/Calc1"),
      force = true).doit()
    report.printArea
    report.printFMax
  }
}

object testCalc1 {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Calc1).
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
