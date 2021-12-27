package Chainsaw

import Chainsaw.matlabIO.MComplex
import breeze.linalg._
import breeze.numerics._

package object algos {

  case class definition() extends scala.annotation.StaticAnnotation

  /** the improved algorithm which reduce the strength of the definition version
   * @param original * @param original the "definition algo" to who it is equivalent
   */
  case class fastAlgo(original:String) extends scala.annotation.StaticAnnotation

  /** the binary algorithm which pointed out the hardware datapath of the definition version
   * @param original the "definition algo" to who it is equivalent
   */
  case class hardAlgo(original: String) extends scala.annotation.StaticAnnotation

  case class matlab() extends scala.annotation.StaticAnnotation

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

}
