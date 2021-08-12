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


}
