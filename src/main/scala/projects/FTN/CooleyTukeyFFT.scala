package projects.FTN

import breeze.numerics.{abs, cos, exp, floor, sin}
import spinal.core._
import spinal.core.sim._
import sysu.xilinx._

import scala.collection.mutable.ArrayBuffer

// N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm
/*
ALGO: DSP with FPGA, algo 6.8, fig 6.12
 */
// TODO: refactor this by functional style
// TODO: inplement interleave function instead of the for loop in part 1 & 3

class CooleyTukeyFFT(N: Int, top: Boolean = true) extends Component {

  val io = new Bundle {
    val input = in Vec(data, N * 2)
    val output = out Vec(data, N * 2)
  }

  val inputNumbers = (0 until N).map(i => ComplexNumber(io.input(i * 2), io.input(i * 2 + 1)))

  val outputNumbers = cooleyTukeyFFT(N, inputNumbers)
  (0 until N).foreach { i =>
    io.output(2 * i) := outputNumbers(i).real.truncated
    io.output(2 * i + 1) := outputNumbers(i).imag.truncated
  }
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

object testCooleyTukeyFFT { // functional verification

  def main(args: Array[String]): Unit = {

    val period = 2

    SimConfig.withWave.compile(new CooleyTukeyFFT(testFFTLength))
      .doSimUntilVoid { dut =>
        fork {
          dut.clockDomain.forkStimulus(period)
        }
        breeze.signal.fourierTr
        val truth8 = Array(28.0, 0, -4, 9.6569, -4, 4, -4, 1.6569, -4, 0, -4, -1.6569, -4, -4, -4, -9.6569)
        sleep(period * 17)
        for (i <- 0 until testFFTLength * 2) dut.io.input(i).raw #= Double2Fix(i.toDouble)
        sleep(period * 5)
        for (i <- 0 until testFFTLength * 2) dut.io.input(i).raw #= (if (i % 2 == 0) Double2Fix((i / 2).toDouble) else 0)
        sleep(period * 10)
        for (i <- 0 until testFFTLength * 2) {
          val output = dut.io.output(i).raw.toBigInt.toDouble / 256.0
          print(output.toString + " ")
          assert(abs(truth8(i) - output) <= data.resolution.toDouble * 10, println(s"golden: ${truth8(i)}, yours: $output"))
        }
        println()
        println("CooleyTukeyFFT verification done")
        simSuccess()
      }
  }
}
