package projects.FTN

import breeze.numerics.{Inf, floor}
import spinal.core._
import spinal.core.sim._

// N-point DFT by Winograd DFT algorithm
/*
algo: On Computing the Discrete Fourier Transform, P18
 */

// TODO: refactor after implementation of ComplexNumber class
// TODO: refactor after implementation of arbitrary N point Winograd DFT

class WinogradDFT (N: Int) extends Component {

  require(isPrime(N), s"Winograd DFT is for prime number")
  require(Set(2).contains(N), s"$N point Winograd DFT will be supported in later release")

  val io = new Bundle{
    val input = in Vec(data, N * 2)
    val output = out Vec(data, N * 2)
  }

  val inputNumbers = (0 until N).map(i => ComplexNumber(io.input(i * 2), io.input(i * 2 + 1)))

  if (N == 2){

    val a0 = inputNumbers(0)
    val a1 = inputNumbers(1)
    val s0 = a0 + a1
    val s1 = a0 - a1
    val m0 = s0
    val m1 = s1

    io.output := Vec(m0.real, m0.imag, m1.real, m1.imag)
  }
}

object WinogradDFT {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new WinogradDFT(4))
  }
}

object testWinogradDFT{
  def Double2Fix(value: Double) = floor(value * (1 << 8)).toInt

  def main(args: Array[String]): Unit = {

    val length = 2

    SimConfig.withWave.compile(new WinogradDFT(length))
      .doSimUntilVoid { dut =>

        for(i <- 0 until length * 2) dut.io.input(i).raw #= Double2Fix(i.toDouble)
        sleep(3)
        for(i <- 0 until length * 2) dut.io.input(i).raw #= Double2Fix((i + length * 2).toDouble)
        sleep(3)
        simSuccess()
      }
  }
}


