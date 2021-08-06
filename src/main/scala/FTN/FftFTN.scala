package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import matlabIO._

class FftFTN {

  def Algo(X: Array[MComplex]): Array[Double] = {

    val N = X.size
    val eng = AsyncEng.get()

    val XReal = X.map(_.real)
    val XImag = X.map(_.imag)

    import scala.math.{sin, cos, Pi}
    def n(k: Int) = sin(k * Pi / N)
    def m(k: Int) = cos(k * Pi / N)

    val YReal = 0.0 +: (1 until N).map(k => (m(k) * XImag(k) - XReal(k)) / n(k))
    val YImag = 0.0 +: (1 until N).map(k => -(m(k) * XReal(k) + XImag(k)) / n(k))

    val Y = YReal.zip(YImag).map { case (real, imag) => new MComplex(real, imag) }.toArray
    val y = eng.feval[Array[MComplex]]("ifft", Y)
    val x = y.map(complex => Array(complex.real, complex.imag)).flatten

    x
  }
}

object FftFTN extends App {

  def apply(): FftFTN = new FftFTN()

  val golden = (0 until 16).map(_ => DSPRand.nextInt(15) + 1).toArray // real sequence
  val X = eng.feval[Array[MComplex]]("fft", golden)
  println(X.mkString(" "))

  val fftFTN = FftFTN()
  val x = fftFTN.Algo(X)

//  println(golden.mkString(" "))
//  println(x.mkString(" "))
}
