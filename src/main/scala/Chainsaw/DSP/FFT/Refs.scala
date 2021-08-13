package Chainsaw.DSP.FFT

import matlabIO._

import scala.util.{Failure, Success, Try}

object Refs {
  val eng = AsyncEng.get()

  def FFT(input: Array[MComplex]): Array[MComplex] = {
    val ret = Try(eng.feval[Array[MComplex]]("fft", input))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("fft", input).map(new MComplex(_, 0))
      case Success(value) => value
    }
  }

  def IFFT(input: Array[MComplex]): Array[MComplex] = {
    val ret = Try(eng.feval[Array[MComplex]]("ifft", input))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("ifft", input).map(new MComplex(_, 0))
      case Success(value) => value
    }
  }

  def cyclicConvolution(input: Array[MComplex], coeff: Array[MComplex], length: Int): Array[MComplex] = {
    val ret = Try(eng.feval[Array[MComplex]]("cconv", input, coeff, Array(length)))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("cconv", input, coeff, Array(length)).map(new MComplex(_, 0))
      case Success(value) => value
    }
  }

  def matIntrlv[T](input: Array[T], row: Int, col: Int) = eng.feval[Array[T]]("matintrlv", input, Array(row), Array(col))
}
