package Chainsaw.DSP.FFT

import matlabIO._
import cc.redberry.rings.scaladsl._

class RaderDFT {

  def Algo(input: Array[MComplex]) = {
    def getGenerator(modulus: Int) = {
      def isGenerator(element: Int): Boolean = (1 until modulus).map(i => Zp(modulus).pow(element, i).intValue()).toSet == (1 until modulus).toSet
      (1 until modulus).dropWhile(!isGenerator(_)).head
    }
    val n = input.size
    val g = getGenerator(n)
    val pows = (0 until n - 1).map(i => Zp(n).pow(g, i).intValue())
  }
}

object RaderDFT extends App {
  def apply(): RaderDFT = new RaderDFT()

  val raderDFT = RaderDFT()
  val input = Array.fill(7)(new MComplex(1, 1))

  raderDFT.Algo(input)
  println(Refs.FFT(input).mkString(" "))
  // properties of WNnk, WNnk rotates around the unit circle
  println(WNnk(16, 1).conj.sameAs(WNnk(16, -1))) // symmetric
  println(WNnk(16, 1).sameAs(WNnk(16, 1 + 16))) // periodic
  println(WNnk(16, 1).sameAs(-WNnk(16, 1 + 16 / 2))) // half-period
  println(WNnk(16, 2).sameAs(WNnk(8, 1))) // ...
}
