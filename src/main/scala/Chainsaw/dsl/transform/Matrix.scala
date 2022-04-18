package Chainsaw.dsl.transform

import Chainsaw.dsl.vectorspace.VectorSpace

import scala.reflect.ClassTag

/** a more accurate name would be "linear transform", as linear transform can always be represented by a matrix, we take matrix as its name
 */
class Matrix[T](val array: Array[Array[T]])
               (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T])
  extends Transform[T, T]((dataIn: Array[T]) => vectorSpace.gemv(array, dataIn), 1, 1) {

  // attributes
  def rows = array.length

  def columns = array.head.length

  override def toString = {
    val widthMax = array.flatten.map(_.toString.length).max
    "matrix:\n" + array.map(_.map(_.toString.padTo(widthMax, ' ')).mkString(" ")).mkString("\n") + s"\n"
  }
}

object Matrix {

  /** basic factory method with full parameters
   */
  def apply[T](array: Array[Array[T]])
              (implicit classTag: ClassTag[T], vectorSpace: VectorSpace[T]): Matrix[T] =
    new Matrix(array)


  /** from an 1-D array, the result is a column vector
   */
  def apply[T](array: Array[T])
              (implicit classTag: ClassTag[T], vectorSpace: VectorSpace[T]): Matrix[T] =
    Matrix(array.map(Array(_)))

  def tabulate[T](m:Int, n:Int)(gen: (Int, Int) => T)
                 (implicit classTag: ClassTag[T], vectorSpace: VectorSpace[T])= {
    Matrix(Array.tabulate(m,n)(gen))
  }
}


