package projects.FTN

import breeze.numerics.floor
import spinal.core._
import spinal.core.sim._

// N-point DFT by Winograd DFT algorithm
/*
algo: On Computing the Discrete Fourier Transform, P18
 */

// TODO: refactor after implementation of ComplexNumber class
// TODO: refactor after implementation of arbitrary N point Winograd DFT

class WinogradDFT (N: Int) extends Component {

  require(Set(2,3,4,5,6).contains(N), s"$N point Winograd DFT is not supported yet")

  def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  val io = new Bundle{
    val input = in Vec(data, N * 2)
    val output = out Vec(data, N * 2)
  }

  if (N == 2){
    val zero = data
    zero := 0.0
    val a0_r = io.input(0)
    val a0_i = io.input(1)
    val a1_r = io.input(2)
    val a1_i = io.input(3)
    val s1_r = a0_r + a1_r
    val s1_i = a0_i + a1_i
    val s2_r = a0_r - a1_r
    val s2_i = a0_i - a1_i
    val m0_r = s1_r
    val m0_i = s1_i
    val m1_r = s2_r
    val m1_i = s2_i

    io.output := Vec(m0_r, m0_i, m1_r, m1_i)
  }

  if (N == 4){ // TODO: fix this
    val zero = data
    zero := 0.0
    val a0_r = io.input(0)
    val a0_i = io.input(1)
    val a1_r = io.input(2)
    val a1_i = io.input(3)
    val a2_r = io.input(4)
    val a2_i = io.input(5)
    val a3_r = io.input(6)
    val a3_i = io.input(7)
    val s1_r = a0_r + a2_r
    val s1_i = a0_i + a2_i
    val s2_r = a0_r - a2_r
    val s2_i = a0_i - a2_i
    val s3_r = a1_r + a3_r
    val s3_i = a1_i + a3_i
    val s4_r = a1_r - a3_r
    val s4_i = a1_i - a3_i
    val s5_r = s1_r + s3_r
    val s5_i = s1_i + s3_i
    val s6_r = s1_r - s3_r
    val s6_i = s1_i - s3_i
    val m1_r = s5_r
    val m1_i = s5_i
    val m2_r = s6_r
    val m2_i = s6_i
    val m3_r = s2_r
    val m3_i = s2_i
    val m4_r = zero - s4_i
    val m4_i = s4_r
    val s7_r = m3_r + m4_r
    val s7_i = m3_i + m4_i
    val s8_r = m3_r - m4_r
    val s8_i = m3_i - m4_i


    io.output := Vec(m1_r, m1_i, s7_r, s7_i, m2_r, m2_i, s8_r, s8_i)
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


