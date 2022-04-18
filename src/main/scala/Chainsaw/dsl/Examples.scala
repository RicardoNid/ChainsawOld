package Chainsaw.dsl

import Chainsaw.{BComplex, dsl}
import breeze.math.Complex
import breeze.numerics.constants.Pi
import breeze.numerics.exp
import spinal.core._
import dsl.field._
import dsl.vectorspace._
import dsl.transform._

object Examples {

  def main(args: Array[String]): Unit = {

    implicit val testField = ComplexField()
    implicit val testSpace = BasicVectorSpace[Complex]

    // software

    def WNnk(nk: Int, N: Int) = exp(Complex(0, -2 * Pi * nk / N))
    val coeff = Array.tabulate(2, 2)((n2, k1) => n2 * k1).flatten.map(WNnk(_, 4))

    val dft2 = Matrix(Array(Array(Complex(1, 0), Complex(1, 0)), Array(Complex(1, 0), Complex(-1, 0))))
    val sp22 = SPermutation(2, 2)
    val t22 = Diagonal(coeff: _*)
    val dft4 = sp22 * dft2.⊗(2) * sp22 * t22 * dft2.⊗(2) * sp22

    println(dft4(Array(Complex(1, 0), Complex(2, 0), Complex(3, 0), Complex(4, 0))).mkString(" "))

    // hardware
  }
}
