package sysu.util

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import sysu.CNN._
import sysu.xilinx._

// (a * b) matrix * (b * c) matrix
class GEMM(widthA: Int, widthB: Int, a: Int, b: Int, c: Int) extends Component {
  val multWidth = widthA + widthB
  val resultWidth = multWidth + log2Up(b)

  val io = new Bundle {
    val matrixA = in Vec(Vec(SInt(widthA bits), b), a)
    val matrixB = in Vec(Vec(SInt(widthB bits), c), b)

    val matrixO = out Vec(Vec(SInt(resultWidth bits), c), a)
  }

  val multRegs = Reg(Vec(Vec(Vec(SInt(multWidth bits), b), c), a)).addAttribute("use_dsp = \"yes\"")
  for (i <- 0 until a; j <- 0 until c; k <- 0 until b) multRegs(i)(j)(k) := io.matrixA(i)(k) * io.matrixB(k)(j)

  val resultRegs = Reg(Vec(Vec(SInt(resultWidth bits), c), a))
  for (i <- 0 until a; j <- 0 until c) {
    resultRegs(i)(j) := SignedAdderTree(multRegs(i)(j), 1)
    io.matrixO(i)(j) := resultRegs(i)(j)
  }

  val latency = LatencyAnalysis(io.matrixA(0)(0), io.matrixO(0)(0))
}

object GEMM {
  def main(args: Array[String]): Unit = {
    if (args(0) == "synth") {
      val report = VivadoFlow(
        design = new GEMM(8, 8, 3, 5, 7),
        vivadoConfig = recommended.vivadoConfig,
        vivadoTask = VivadoTask(topModuleName = "GEMM", workspacePath = "output/GEMM", frequencyTarget = 600 MHz, taskType = SYNTH),
        force = true).doit()
      report.printArea
      report.printFMax
    }
    else if (args(0) == "sim") {
      val period = 2
      SimConfig.withWave.compile(new GEMM(8, 8, 3, 5, 7))
        .doSimUntilVoid { dut =>
          fork {
            dut.clockDomain.forkStimulus(period = period)
          }
          sleep(period * 17)
          sleep(period / 2)
          for (i <- 0 until 3; j <- 0 until 5) dut.io.matrixA(i)(j) #= i + j
          for (i <- 0 until 5; j <- 0 until 7) dut.io.matrixB(i)(j) #= i + j
          sleep(period * 50)
          simSuccess()
        }
    }
  }
}




