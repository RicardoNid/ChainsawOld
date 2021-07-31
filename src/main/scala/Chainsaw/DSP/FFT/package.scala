package Chainsaw.DSP

import matlabIO._

package object FFT {
  val eng = AsyncEng.get()
  def FFTRef(input: Array[MComplex]): Array[MComplex] = {
    eng.feval[Array[MComplex]]("fft", input)
  }

  import scala.math.Pi
  // FIXME: matlab return double when the imaginary part is 0
  def WNnk(N:Int, n:Int, k:Int) = eng.feval[MComplex]("exp", new MComplex(0, 2 * Pi * k * n / N))
  def WNnk(N:Int, nk:Int) = eng.feval[MComplex]("exp", new MComplex(0, 2 * Pi * nk / N))
}
