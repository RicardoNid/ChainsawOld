package projects.FTN

import breeze.numerics.{abs, cos, exp, floor, sin}
import spinal.core._
import spinal.core.sim._
import sysu.xilinx._

import scala.collection.mutable.ArrayBuffer

// N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm
// TODO: refactor this by functional style
// TODO: inplement interleave function instead of the for loop in part 1 & 3

class CooleyTukeyFFT(N: Int, top: Boolean = true) extends Component {

  def factorize(N: Int): ArrayBuffer[Int] = {
    if (isPrime(N)) ArrayBuffer(N)
    else {
      val factor = (2 until N).find(N % _ == 0).get
      val result = factorize(N / factor)
      result.insert(0, factor)
      result
    }
  }

  // W_{N}^{k n} = \mathrm{e}^{-\mathrm{j} 2 \pi k n / N}
  def coefficientW(n2: Int, k1: Int): (Double, Double) = {
    val real = cos(-2 * math.Pi * n2 * k1 / N)
    val imag = sin(-2 * math.Pi * n2 * k1 / N)
    (real, imag)
  }

  val io = new Bundle {
    val input = in Vec(data, N * 2)
    val output = out Vec(data, N * 2)
  }

  val inputNumbers =  (0 until N).map(i => ComplexNumber(io.input(i * 2), io.input(i * 2 + 1)))

  val factors = factorize(N)
  val firstFactor = factors(0)
  println(s"the implementation factorized as $factors")

  if (factors.length == 1) {

    val outputNumbers = winogradDFT(firstFactor, inputNumbers)
    (0 until N).foreach{ i =>
      io.output(2 * i) := outputNumbers(i).real
      io.output(2 * i + 1) := outputNumbers(i).imag
    }
  }
  else {

    val restFactor = factors.drop(1).reduce(_ * _)
    println(restFactor)

    val winos: Array[WinogradDFT] = Array.fill(restFactor)(new WinogradDFT(firstFactor))
    val rests: Array[CooleyTukeyFFT] = Array.fill(firstFactor)(new CooleyTukeyFFT(restFactor, top = false))

    // part 1, input permutation and N1-point DFTs
    for (i <- 0 until firstFactor; j <- 0 until restFactor; k <- 0 until 2) {
      rests(i).io.input(j * 2 + k) := RegNext(io.input(j * firstFactor * 2 + i * 2 + k))
      //      rests(i).io.input(j * 2 + k) := io.input(j * firstFactor * 2 + i * 2 + k)
      if (top) println(s"x[${j * firstFactor * 2 + i * 2 + k}] to [$i, $j, $k]")
    }

    for (i <- 0 until restFactor; j <- 0 until firstFactor) { // part 2, twiddle
      val twiddleFactor = coefficientW(n2 = j, k1 = i)
      val (real, imag) = complexMultiplier(
        twiddleFactor._1, twiddleFactor._2,
        rests(j).io.output(i * 2 + 0), rests(j).io.output(i * 2 + 1))
      winos(i).io.input(j * 2 + 0) := RegNext(real).truncated
      //      winos(i).io.input(j * 2 + 0) := real
      winos(i).io.input(j * 2 + 1) := RegNext(imag).truncated
      //      winos(i).io.input(j * 2 + 1) := imag
    }

    for (i <- 0 until restFactor; j <- 0 until firstFactor; k <- 0 until 2) { // part 3, N2-point DFTs
      io.output(j * restFactor * 2 + i * 2 + k) := RegNext(winos(i).io.output(j * 2 + k))
      //      io.output(j * restFactor * 2 + i * 2 + k) := winos(i).io.output(j * 2 + k)
      if (top) println(s"[$i, $j, $k] to X[${j * restFactor * 2 + i * 2 + k}]")
    }




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
        val truth8 = Array(28.0, 0, -4, 9.6569, -4, 4, -4, 1.6569, -4, 0, -4, -1.6569, -4, -4, -4, -9.6569)
        sleep(period * 17)
        for (i <- 0 until testFFTLength * 2) dut.io.input(i).raw #= Double2Fix(i.toDouble)
        sleep(period * 5)
        for (i <- 0 until testFFTLength * 2) dut.io.input(i).raw #= (if (i % 2 == 0) Double2Fix((i / 2).toDouble) else 0)
        sleep(period * 10)
        for (i <- 0 until testFFTLength * 2) {
          val output = dut.io.output(i).raw.toBigInt.toDouble / 256.0
          println(output)
          assert(abs(truth8(i) - output) <= data.resolution.toDouble * 10, println(abs(truth8(i) - output)))
        }
        simSuccess()
      }
  }
}
