package projects.huang

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import sysu.CNN._
import sysu.util._



class Calc(loopNestConv: LoopNestConv) extends Component {

  import loopNestConv._

  val io = new Bundle {
    val inputDataIn = slave(Stream(Vec(word_t, Pif)))
    val weightDataIn = slave(Stream(Vec(Vec(word_t, Pif), Pof)))

    val calc2write_1 = master(Stream(Vec(word_t, Pof)))
  }

  // todo : 为简化设计,暂时不做DSP暂停,加深对DSP的了解之后再做改进
  // datapath
  val multRegs = Vec(Vec(double_word_t, Pif), Pof).addAttribute("use_dsp = \"yes\"")
  for (n <- 0 until Pof; c <- 0 until Pif) {
    multRegs(n)(c) := io.inputDataIn.payload(c) * io.weightDataIn.payload(n)(c)
  }
  val adderTrees = (0 until Pif).map(i => AdderTree(multRegs(i), 1))
  val accRegs = RegInit(Vec((0 until Pof).map(_ => U(0, dataWidth * 2 + log2Up(Nif) bits)), output_t))

  // control
  io.inputDataIn.ready := io.calc2write_1.ready
  io.weightDataIn.ready := io.calc2write_1.ready
  val validCounter = Counter(0 until (Nif / Pif), io.inputDataIn.fire && io.weightDataIn.fire)
  when(validCounter.willOverflow)((0 until Pof).foreach(i => accRegs(i) := adderTrees(i).resized))
    .otherwise((0 until Pof).foreach(i => accRegs(i) := accRegs(i) + adderTrees(i)))

  io.calc2write_1.payload := accRegs.resized // todo : 更加精细地处理截断问题
  val latency = LatencyAnalysis(io.inputDataIn.payload(0), io.calc2write_1.payload(0))
  io.calc2write_1.valid := Delay(validCounter.willOverflow, latency, init = False)

  println("Datapath delay " + LatencyAnalysis(io.inputDataIn.payload(0), io.calc2write_1.payload(0)))
  println("Ctrl delay " + LatencyAnalysis(io.inputDataIn.valid, io.calc2write_1.valid))
  println("Ctrl delay " + LatencyAnalysis(io.calc2write_1.ready, io.inputDataIn.ready))
}

object Calc {
  def main(args: Array[String]): Unit = {

    SpinalSystemVerilog(new Calc(loopNestHuang1))

    //    val moduleName = "Calc1"
    //    val report = VivadoFlow(
    //      design = new Calc(loopNestHuang1),
    //      vivadoConfig = recommended.vivadoConfig,
    //      vivadoTask = VivadoTask(
    //        topModuleName = moduleName,
    //        workspacePath = s"output/huang/Calc1"),
    //      force = true).doit()
    //    report.printArea
    //    report.printFMax
  }
}

object testCalc {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Calc(loopNestHuang1)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {

          dut.io.inputDataIn.valid #= true
          dut.io.weightDataIn.valid #= true
          dut.io.calc2write_1.ready #= true

          (0 until loopNestHuang1.Pif).foreach(i => dut.io.inputDataIn.payload(i) #= i)
          for (n <- 0 until loopNestHuang1.Pof; c <- 0 until loopNestHuang1.Pif) dut.io.weightDataIn.payload(n)(c) #= n

          sleep(1000 * period)
          simSuccess()
        }
      }
  }
}