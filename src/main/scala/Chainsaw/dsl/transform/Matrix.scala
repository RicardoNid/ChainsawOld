package Chainsaw.dsl.transform

import Chainsaw.dsl
import spinal.core.{Bits, _}
import Chainsaw.dsl._

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

/** a more accurate name would be "linear transform", as linear transform can always be represented by a matrix, we take matrix as its name
 */
class Matrix[T: ClassTag](val array: Array[Array[T]])
                         (implicit ring: Ring[T])
  extends BaseTransform[T, T](Matrix.getAlgo(array), new MatrixImpl(array.map(_.map(ring.toBits)), ring.multH, ring.addH)) {

  override def toString = {
    val widthMax = array.flatten.map(_.toString.length).max
    "matrix:\n" + array.map(_.map(_.toString.padTo(widthMax, ' ')).mkString(" ")).mkString("\n") + s"\n"
  }
}

object Matrix {

  @tailrec
  def reduceBalancedTree[T:ClassTag](array: Array[T], op: Op2[T]): Array[T] = {
    val n = array.length
    if (n == 1) array
    else {
      val (left, right) = array.splitAt(n / 2)
      val sums = left.zip(right).map { case (a, b) => op(a, b) }
      val remain = if (n % 2 == 1) Array(array.last) else Array.empty[T]
      reduceBalancedTree(sums ++ remain, op)
    }
  }

  def getAlgo[T: ClassTag](array: Array[Array[T]])
                          (implicit ring: Ring[T]) =
    (dataIn: Array[T]) => {
      val afterMult = array.map(row => row.zip(dataIn).map { case (coeff, data) => ring.mult(coeff, data) })
      afterMult.map(reduceBalancedTree(_, ring.add)).map(_.head)
    }

  def getImpl[T: ClassTag](array: Array[Array[T]])
                          (implicit ring: Ring[T])
  =  Impl(
    size = (array.head.length, array.length),
    impl = (dataIn: Vec[Bits]) => {

      val coeffs = array.map(_.map(ring.toCoeff))
      val afterMult = coeffs.map(row => row.zip(dataIn).map { case (coeff, data) => ring.multH.op(coeff, data) })
      Vec(afterMult.map(reduceBalancedTree(_, ring.addH.op)).map(_.head))
    },
    latency = log2Up(array.head.length) * ring.addH.latency + ring.multH.latency
  )

  /** basic factory method with full parameters
   */
  def apply[T: ClassTag]
  (array: Array[Array[T]])
  (implicit ring: Ring[T]): Matrix[T] =
    new Matrix(array)

  /** from an 1-D array, the result is a column vector
   */
  def apply[T: ClassTag]
  (array: Array[T])
  (implicit ring: Ring[T]): Matrix[T] =
    Matrix(array.map(Array(_)))

  def tabulate[T: ClassTag]
  (m: Int, n: Int)(gen: (Int, Int) => T)
  (implicit ring: Ring[T]) = {
    Matrix(Array.tabulate(m, n)(gen))
  }
}


