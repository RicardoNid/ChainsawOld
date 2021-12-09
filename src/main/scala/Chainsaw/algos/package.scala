package Chainsaw

import breeze.linalg._
import breeze.numerics._

package object algos {

  case class definition() extends scala.annotation.StaticAnnotation
  case class fastAlgo() extends scala.annotation.StaticAnnotation

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

    def ~= (that: DenseVector[BComplex]) = {
      val epsilon = 1E-4
      abs(dv - that).forall(_ < epsilon)
    }

    def toMatlab = dv.toArray.map(_.toMComplex)
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

    def ~= (that: DenseVector[Double]) = {
      val epsilon = 1E-4
      abs(dv - that).forall(_ < epsilon)
    }
  }

}
