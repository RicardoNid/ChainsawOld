package Chainsaw.dsl.transform.base

import Chainsaw.dsl.vectorspace.VectorSpace
import spinal.core.{Bits, _}
import Chainsaw.dsl._

import scala.reflect.{ClassTag, classTag}

/** a more accurate name would be "linear transform", as linear transform can always be represented by a matrix, we take matrix as its name
 */
class Matrix[T: ClassTag](val array: Array[Array[T]])
                         (implicit vectorSpace: VectorSpace[T])
  extends BaseTransform[T, T](Matrix.getAlgo(array), Matrix.getImpl(array))(vectorSpace.field, vectorSpace.field) {

  override def toString = {
    val widthMax = array.flatten.map(_.toString.length).max
    "matrix:\n" + array.map(_.map(_.toString.padTo(widthMax, ' ')).mkString(" ")).mkString("\n") + s"\n"
  }
}

object Matrix {

  def getAlgo[T](array: Array[Array[T]])
                (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]) =
    (dataIn: Array[T]) => vectorSpace.gemv(array, dataIn)

  def getImpl[T](array: Array[Array[T]])
                (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T])
  = new Impl(
    size = (array.head.length, array.length),
    impl = (dataIn: Vec[Bits]) => Vec(vectorSpace.gemv(array, dataIn.toArray)),
    latency = 1 // TODO: correct latency
  )

  /** basic factory method with full parameters
   */
  def apply[T: ClassTag](array: Array[Array[T]])
                        (implicit vectorSpace: VectorSpace[T], field: MixType[T]): Matrix[T] =
    new Matrix(array)

  /** from an 1-D array, the result is a column vector
   */
  def apply[T: ClassTag](array: Array[T])
                        (implicit vectorSpace: VectorSpace[T], field: MixType[T]): Matrix[T] =
    Matrix(array.map(Array(_)))

  def tabulate[T: ClassTag]
  (m: Int, n: Int)(gen: (Int, Int) => T)
  (implicit vectorSpace: VectorSpace[T], field: MixType[T]) = {
    Matrix(Array.tabulate(m, n)(gen))
  }
}


