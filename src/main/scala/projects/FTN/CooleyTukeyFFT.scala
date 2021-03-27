package projects.FTN

import spinal.core._
import spinal.lib._
import sysu.xilinx._

// N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm
/*
ALGO: DSP with FPGA, algo 6.8, fig 6.12
 */
// TODO: refactor this by functional style
// TODO: inplement interleave function instead of the for loop in part 1 & 3

class CooleyTukeyFFT(N: Int) extends Component {

  val io = new Bundle {
    val input = slave Flow (Vec(data, N * 2))
    val output = master Flow (Vec(data, N * 2))
  }

  val inputNumbers = (0 until N).map(i => ComplexNumber(io.input.payload(i * 2), io.input.payload(i * 2 + 1)))

  val outputNumbers = cooleyTukeyFFT(inputNumbers)
  (0 until N).foreach { i =>
    io.output.payload(2 * i) := outputNumbers(i).real.truncated
    io.output.payload(2 * i + 1) := outputNumbers(i).imag.truncated
  }

  // TODO: find a way to determine the latency automatically
  io.output.valid := Delay(io.input.valid, 2)
  io.output.valid.init(False)
}

object CooleyTukeyFFT {
  def main(args: Array[String]): Unit = {

    val task = VivadoTask(
      topModuleName = "FFT",
      workspacePath = "./output/FTN",
      frequencyTarget = (600 MHz)
    )

    val report = VivadoFlow( // performance verification
      design = new CooleyTukeyFFT(testFFTLength),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = task,
      force = true
    ).doit()

    println(s"DSP estimated = , DSP consumed = ${report.DSP}")
    println(s"frequency expected = 600 MHz, frequency met = ${report.Frequency / 1E6} MHz")
    report.printArea
    report.printFMax
  }
}