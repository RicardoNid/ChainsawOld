package Chainsaw.DSP.FFT


import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite

class FFTAlgosTest extends AnyFunSuite {

  val eng = AsyncEng.get()
  def same(a: Seq[MComplex], b: Seq[MComplex]) = a.zip(b).forall { case (complex, complex1) => complex.sameAs(complex1) }

  test("testCooleyTukey") {
    val test75 = randComplex(75)
    val factors = Seq(3, 5, 5)
    assert(same(Algos.cooleyTukeyFFT(test75, factors), Refs.FFT(test75)))
    printlnGreen(s"test CTFFT ${factors.mkString("*")} passed")
  }

  test("testRadixR") {
    val test64 = randComplex(64)
    assert(same(Algos.radixRFFT(test64, 2), Refs.FFT(test64)))
    printlnGreen(s"test radix-2 FFT passed")
  }

}
