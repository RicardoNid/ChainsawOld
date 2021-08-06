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
  println(swapByButterfly((0 until 8)))

  printlnGreen("coeffs")
  println(radixRCoeff(8,2,8).mkString(" "))

  printlnGreen("FFT")
  val eng = AsyncEng.get()
  val test = (0 until 32).map(_ => (DSPRand.nextDouble() - 0.5) * 16)
  val testComplex = (0 until 16).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray
  println(s"fft input:    ${testComplex.mkString(" ")}")
  println(s"your radix-2: ${radix2FFT(testComplex).mkString(" ")}")
  println(s"your radix-4: ${radix4FFT(testComplex).mkString(" ")}")
  println(s"golden:       ${eng.feval[Array[MComplex]]("fft", testComplex).mkString(" ")}")


}
