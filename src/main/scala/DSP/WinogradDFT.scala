package DSP

import DSP.WinogradDFT.winogradDFT
import breeze.numerics.{Inf, floor}
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, master, slave}

// N-point DFT by Winograd DFT algorithm
/*
algo: On Computing the Discrete Fourier Transform, P18
 */

// TODO: refactor after implementation of ComplexNumber class
// TODO: refactor after implementation of arbitrary N point Winograd DFT

class WinogradDFT (N: Int) extends Component {

  require(isPrime(N), s"Winograd DFT is for prime number")
  require(Set(2).contains(N), s"$N point Winograd DFT will be supported in later release")

  val io = new Bundle {
    val input = slave Flow (Vec(data, N * 2))
    val output = master Flow (Vec(data, N * 2))
  }

  val inputNumbers = (0 until N).map(i => ComplexNumber(io.input.payload(i * 2), io.input.payload(i * 2 + 1)))

  val outputNumbers = winogradDFT(inputNumbers)
  (0 until N).foreach { i =>
    io.output.payload(2 * i) := outputNumbers(i).real.truncated
    io.output.payload(2 * i + 1) := outputNumbers(i).imag.truncated
  }

  io.output.valid := Delay(io.input.valid, 1)
  io.output.valid.init(False)
}

object WinogradDFT {

  def winogradDFT(input: IndexedSeq[ComplexNumber]) = {

    val N = input.length

    require(isPrime(N), s"Winograd DFT is for prime number")
    require(Set(2).contains(N), s"$N point Winograd DFT will be supported in later release")

    val output = Array.ofDim[ComplexNumber](N)
    if (N == 2) {
      val s0 = input(0) + input(1)
      val s1 = input(0) - input(1)
      val m0 = s0
      val m1 = s1
      output(0) = m0
      output(1) = m1
    }

    //    if(N == 3){
    //      val s1 = input(1) + input(2)
    //      val s2 = input(1) - input(2)
    //      val s3 = input(0) - input(1)
    //
    //      val m0 = s3
    //      val m1 = s1 * ComplexNumber(cos(2 * Pi / 3) - 1, 0)
    //      val m2 = s2 * ComplexNumber(cos(2 * Pi / 3) - 1, 0)
    //    }

    output.map(_.tap)
  }

  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new WinogradDFT(4))
  }
}


