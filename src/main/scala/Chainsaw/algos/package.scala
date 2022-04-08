package Chainsaw

import Chainsaw.matlabIO.MComplex
import breeze.linalg._
import breeze.numerics._

package object algos {

  case class Modulo(modulo: Int)

  // TODO: generic
  implicit class dvComplexUtil(dv: DenseVector[BComplex]) {

    def reverse = new DenseVector(dv.toArray.reverse)

    def rotateLeft(i: Int): DenseVector[BComplex] = {
      val (l, r) = dv.toArray.splitAt(i)
      new DenseVector(r ++ l)
    }

    def rotateRight(i: Int): DenseVector[BComplex] = {
      val (l, r) = dv.toArray.splitAt(dv.length - i)
      new DenseVector(r ++ l)
    }

    def ~=(that: DenseVector[BComplex], epsilon:Double = 1E-4) = {
      (dv - that).forall(complex => complex.real < epsilon && complex.imag < epsilon)
    }

    def toMatlab: Array[MComplex] = dv.toArray.map(_.toMComplex)
  }

  implicit class dvDoubleUtil(dv: DenseVector[Double]) {

    def reverse = new DenseVector(dv.toArray.reverse)

    def rotateLeft(i: Int): DenseVector[Double] = {
      val (l, r) = dv.toArray.splitAt(i)
      new DenseVector(r ++ l)
    }

    def rotateRight(i: Int): DenseVector[Double] = {
      val (l, r) = dv.toArray.splitAt(dv.length - i)
      new DenseVector(r ++ l)
    }

    def ~=(that: DenseVector[Double]) = {
      val epsilon = 1E-4
      abs(dv - that).forall(_ < epsilon)
    }
  }

  def bits2Symbols(bits: DenseVector[Int], symbolSize: Int) = {
    val symbols: Array[Int] = bits.toArray.grouped(symbolSize).toArray
      .map(_.mkString("")).map(BigInt(_, 2).toInt)
    new DenseVector(symbols)
  }

  def symbols2Bits(symbols: DenseVector[Int], symbolSize: Int) = {
    val bits = symbols.toArray.flatMap(_.toBinaryString.padToLeft(symbolSize, '0').map(_.asDigit))
    new DenseVector(bits)
  }

}
