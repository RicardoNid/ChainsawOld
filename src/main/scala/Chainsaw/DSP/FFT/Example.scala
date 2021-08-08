package Chainsaw.DSP.FFT

import Chainsaw.{DSPRand, printlnGreen}
import matlabIO._

object Example extends App {

  printlnGreen("bitReverse")
  val bitReverse = (0 until 8).map(digitReverse(_, 2, 3))
  val reverseBack = bitReverse.map(digitReverse(_, 2, 3))
  println(s"radix-2 bitReverse  : ${bitReverse.mkString(" ")}\n${reverseBack.mkString(" ")}\n")
  println(s"radix-4 digitReverse: ${(0 until 16).map(digitReverse(_, 4, 4)).mkString(" ")}")

  printlnGreen("butterfly")
  println(swapByButterfly(0 until 8))

  printlnGreen("coeffs")
  println(radixRCoeff(8, 2, 8).mkString(" "))

  printlnGreen("FFT")
  val eng = AsyncEng.get()
  def randComplex(length: Int) = {
    val test = (0 until 2 * length).map(_ => (DSPRand.nextDouble() - 0.5) * 16)
    (0 until length).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray
  }
  val testComplex = randComplex(16)
  println(s"fft input:    ${testComplex.mkString(" ")}")
  println(s"your radix-2: ${radix2FFT(testComplex).mkString(" ")}")
  println(s"your radix-4: ${radix4FFT(testComplex).mkString(" ")}")
  println(s"golden:       ${eng.feval[Array[MComplex]]("fft", testComplex).mkString(" ")}")

  printlnGreen("CT 3*4")
  val test75 = randComplex(75)
  println(s"your ct 3 * 4: ${ctFFT(test75).mkString(" ")}")
  println(s"golden:        ${eng.feval[Array[MComplex]]("fft", test75).mkString(" ")}")

}
