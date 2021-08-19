package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite

class FFTAlgosTest extends AnyFunSuite {

  def same(a: Seq[MComplex], b: Seq[MComplex]) = a.zip(b).forall { case (complex, complex1) => complex.sameAs(complex1) }
  def testAlgo(N: Int, algo: Seq[MComplex] => Seq[MComplex]) = {
    val testCase = randComplex(N)
    println(algo(testCase).mkString(" ")) // to show that the algo really did something
    assert(same(algo(testCase), Refs.FFT(testCase)))
  }

  test("test fast algos for DFT") {
    val Ns0 = Seq(24, 75, 133)
    val factors = Seq(Seq(4, 6), Seq(3, 5, 5), Seq(7, 19))
    val algos = factors.map(factor => Algos.cooleyTukeyFFT(_: Seq[MComplex], factor))
    Ns0.zip(algos).foreach { case (n, algo) => testAlgo(n, algo) }
    printlnGreen(s"test CooleyTukey passed")

    val Ns1 = Seq(16, 32, 64, 128, 256)
    Ns1.foreach(testAlgo(_, Algos.radixRFFT(_: Seq[MComplex], 2)))
    printlnGreen(s"test radix-2 FFT passed")
    Seq(16, 64, 256).foreach(testAlgo(_, Algos.radixRFFT(_: Seq[MComplex], 4)))
    printlnGreen(s"test radix-4 FFT passed")

    val Ns2 = Seq(5, 7, 11, 13, 17, 19) // primes
    Ns2.foreach(testAlgo(_, Algos.raderDFT))
    printlnGreen(s"test Rader DFT passed")
  }

  test("CooleyTukey fast test"){
    val Ns0 = Seq(24)
    val factors = Seq(Seq(4, 6))
    val algos = factors.map(factor => Algos.cooleyTukeyFFT(_: Seq[MComplex], factor))
    Ns0.zip(algos).foreach { case (n, algo) => testAlgo(n, algo) }
    printlnGreen(s"test CooleyTukey passed")
  }

  test("show ctfft"){
    val testCase = (0 until 8).map(i => new MComplex(i,i))
    Algos.cooleyTukeyFFT(testCase, Seq(4,2))
  }
}
