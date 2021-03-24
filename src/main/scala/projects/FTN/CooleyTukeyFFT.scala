package projects.FTN

import breeze.numerics.{cos, floor, sin}
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

// N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm
// TODO: refactor this by functional style
// TODO: inplement interleave function instead of the for loop in part 1 & 3

class CooleyTukeyFFT(N: Int, top: Boolean = true) extends Component {

  def isprime(N: Int) = {
    if (Set(2, 3, 5, 7, 11, 13).contains(N)) true
    else false
  }

  def factorize(N: Int): ArrayBuffer[Int] = {
    if (isprime(N)) ArrayBuffer(N)
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

  val factors = factorize(N)

  if (factors.length == 1) {
    val wino = new WinogradDFT(N)
    io.input <> wino.io.input
    io.output <> wino.io.output
  }
  else {
    val firstFactor = factors(0)
    val restFactor = factors.drop(1).reduce(_ * _)
    println(restFactor)
    val winos: Array[WinogradDFT] = Array.fill(restFactor)(new WinogradDFT(firstFactor))
    val rests: Array[CooleyTukeyFFT] = Array.fill(firstFactor)(new CooleyTukeyFFT(restFactor, top = false))

    for (i <- 0 until firstFactor; j <- 0 until restFactor; k <- 0 until 2) { // part 1, N1-point DFTs
      rests(i).io.input(j * 2 + k) := io.input(j * firstFactor * 2 + i * 2 + k)
      if (top) println(s"x[${j * firstFactor * 2 + i * 2 + k}] to [$i, $j, $k]")
    }

    for (i <- 0 until restFactor; j <- 0 until firstFactor) { // part 2, twiddle
      val twiddleFactor = coefficientW(n2 = j, k1 = i)
      val (real, imag) = complexMultiplier(
        twiddleFactor._1, twiddleFactor._2,
        rests(j).io.output(i * 2 + 0), rests(j).io.output(i * 2 + 1))
      winos(i).io.input(j * 2 + 0) := real
      winos(i).io.input(j * 2 + 1) := imag
    }

    for (i <- 0 until restFactor; j <- 0 until firstFactor; k <- 0 until 2) { // part 3, N2-point DFTs
      io.output(j * restFactor * 2 + i * 2 + k) := winos(i).io.output(j * 2 + k)
      if (top) println(s"[$i, $j, $k] to X[${j * restFactor * 2 + i * 2 + k}]")
    }
  }
}

object CooleyTukeyFFT {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new CooleyTukeyFFT(4))
  }
}

object testCooleyTukeyFFT {

  def main(args: Array[String]): Unit = {

    val length = 8

    SimConfig.withWave.compile(new CooleyTukeyFFT(length))
      .doSimUntilVoid { dut =>

        for (i <- 0 until length * 2) dut.io.input(i).raw #= Double2Fix(i.toDouble)
        sleep(3)
        for (i <- 0 until length * 2) dut.io.input(i).raw #= (if (i % 2 == 0) Double2Fix((i / 2).toDouble) else 0)
        sleep(3)
        simSuccess()
      }
  }
}
