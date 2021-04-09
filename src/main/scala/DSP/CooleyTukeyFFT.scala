package DSP

import DSP.CooleyTukeyFFT.cooleyTukeyFFT
import DSP.WinogradDFT.winogradDFT
import breeze.numerics.constants.Pi
import breeze.numerics.{cos, sin}
import spinal.core._
import spinal.lib._
import xilinx.{VivadoFlow, VivadoReport, VivadoTask, recommended}


/** N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm, DSP with FPGA, algo 6.8, fig 6.12
 * @param N length of FFT
 */
class CooleyTukeyFFT(N: Int) extends Component {

  val io = new Bundle {
    val input = slave Flow (Vec(globalType, N * 2))
    val output = master Flow (Vec(globalType, N * 2))
  }

  val inputNumbers = (0 until N).map(i => ComplexNumber(io.input.payload(i * 2), io.input.payload(i * 2 + 1)))

  val outputNumbers = cooleyTukeyFFT(inputNumbers)
  (0 until N).foreach { i =>
    io.output.payload(2 * i) := outputNumbers(i).real.truncated
    io.output.payload(2 * i + 1) := outputNumbers(i).imag.truncated
  }

  // TODO: find a way to determine the latency automatically
  io.output.valid := Delay(io.input.valid, 2 * factorize(N).length - 1, init = False)
  io.output.valid.init(False)
}

object CooleyTukeyFFT {

  def cooleyTukeyFFT(input: IndexedSeq[ComplexNumber]): IndexedSeq[ComplexNumber] = {

    val N = input.length

    val factors = factorize(N)
    val factor1 = factors(0)
    val factor2 = factors.reduce(_ * _) / factor1

    val outputNumbers = Array.ofDim[ComplexNumber](N)

    if (isPrime(N)) (0 until N).foreach(i => outputNumbers(i) = winogradDFT(input)(i))
    else {

      // W_{N}^{k n} = \mathrm{e}^{-\mathrm{j} 2 \pi k n / N}
      def coefficient(k1: Int, n2: Int) = ComplexNumber(cos(-2 * Pi * n2 * k1 / N), sin(-2 * Pi * n2 * k1 / N))

      val coefficients = Array.tabulate(factor2, factor1)((k1, n2) => coefficient(k1, n2)).flatten

      val cooleyGroups = input.zipWithIndex.sortBy(_._2 % factor1).map(_._1).grouped(factor2).toArray

      val stage1Numbers = cooleyGroups .map(cooleyTukeyFFT(_)).flatten
      stage1Numbers.zipWithIndex.foreach{case (number, index) => number.real.setName(s"stage1Numbers_real_$index")}
      val stage2Numbers = stage1Numbers.zipWithIndex.sortBy(_._2 % factor2).map(_._1).zip(coefficients).map { case (signal, coeff) => signal * coeff }.map(_.tap)

      val winoGroups = stage2Numbers.grouped(factor1).toArray
      val winoResults = winoGroups.map(winogradDFT(_)).flatten

      (0 until N).foreach(i => outputNumbers(i % factor1 * factor2 + i / factor1) = winoResults(i))
    }

    outputNumbers
  }

  def main(args: Array[String]): Unit = {

    val task = VivadoTask(
      frequencyTarget = (600 MHz)
    )

    def testPerformance(length: Int) = VivadoFlow( // performance verification
      design = new CooleyTukeyFFT(length),
      topModuleName = "FFT",
      workspacePath = "./output/FTN",
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = task,
      force = true
    ).doit()

    def doReport(report: VivadoReport) = {
      println(s"DSP estimated = , DSP consumed = ${report.DSP}")
      println(s"FF estimated = ${bitWidth * testFFTLength * (factorize(testFFTLength).length * 2 - 1)}, FF consumed = ${report.FF}")
      println(s"frequency expected = 600 MHz, frequency met = ${report.Frequency / 1E6} MHz")
      report.printArea
      report.printFMax
    }

    Array(4, 8, 16).map(testPerformance(_)).foreach(doReport(_))
  }
}