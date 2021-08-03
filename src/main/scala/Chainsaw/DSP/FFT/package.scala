package Chainsaw.DSP

import matlabIO._

import scala.util.{Failure, Success, Try}

package object FFT {
  val eng = AsyncEng.get()
  def FFTRef(input: Array[MComplex]): Array[MComplex] = {
    val ret = Try(eng.feval[Array[MComplex]]("fft", input))
    ret match {
      case Failure(exception) => eng.feval[Array[Double]]("fft", input).map(new MComplex(_, 0))
      case Success(value) => value
    }
  }

  import scala.math.Pi

  // FIXME: matlab return double when the imaginary part is 0
  def WNnk(N: Int, nk: Int): MComplex = {
    val ret = Try(eng.feval[MComplex]("exp", new MComplex(0, 2 * Pi * nk / N))) // TODO: Pi may not be accurate enough
    ret match {
      case Failure(exception) => new MComplex(eng.feval[Double]("exp", new MComplex(0, 2 * Pi * nk / N)), 0)
      case Success(value) => value
    }
  }
  def WNnk(N: Int, n: Int, k: Int): MComplex = WNnk(N, n * k)
}
