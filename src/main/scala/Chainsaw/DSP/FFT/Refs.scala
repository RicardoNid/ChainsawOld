package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.matlabIO._

import scala.util.{Failure, Success, Try}

object Refs {
  val eng = AsyncEng.get()

  def FFT(input: Array[BComplex]): Array[BComplex] = {
    val ret = Try(eng.feval[Array[MComplex]]("fft", input.map(_.toMComplex)))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("fft", input.map(_.toMComplex)).map(new BComplex(_, 0))
      case Success(value) => value.map(_.toBComplex)
    }
  }

  def IFFT(input: Array[BComplex]): Array[BComplex] = {
    val ret = Try(eng.feval[Array[MComplex]]("ifft", input.map(_.toMComplex)))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("ifft", input.map(_.toMComplex)).map(new BComplex(_, 0))
      case Success(value) => value.map(_.toBComplex)
    }
  }

  def cyclicConvolution(input: Array[BComplex], coeff: Array[BComplex], length: Int): Array[BComplex] = {
    val ret = Try(eng.feval[Array[BComplex]]("cconv", input, coeff, Array(length)))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("cconv", input, coeff, Array(length)).map(new BComplex(_, 0))
      case Success(value) => value
    }
  }
}
