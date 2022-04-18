package Chainsaw.dsl.transform

import Chainsaw.dsl.vectorspace.VectorSpace
import spinal.core.{Bits, Data}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.reflect.ClassTag
import Matrix._

/** a more accurate name would be "linear transform", as linear transform can always be represented by a matrix, we take matrix as its name
 */
class Matrix[T](val array: Array[Array[T]])
               (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T])
  extends Transform[T, T](
    Matrix.transform(array), Matrix.impl(array),
    vectorSpace.field.width, array.length, array.head.length,
    1, 1) {

  // attributes
  def rows = array.length

  def columns = array.head.length

  override def toString = {
    val widthMax = array.flatten.map(_.toString.length).max
    "matrix:\n" + array.map(_.map(_.toString.padTo(widthMax, ' ')).mkString(" ")).mkString("\n") + s"\n"
  }
}

object Matrix {

  def transform[T](array: Array[Array[T]])
                  (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]) =
    (dataIn: Array[T]) => vectorSpace.gemv(array, dataIn)

  def impl[T](array: Array[Array[T]])
             (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T])
  = (dataIn: Vec[Bits]) => Vec(vectorSpace.gemv(array, dataIn.toArray))

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

  def tabulate[T](m: Int, n: Int)(gen: (Int, Int) => T)
                 (implicit classTag: ClassTag[T], vectorSpace: VectorSpace[T]) = {
    Matrix(Array.tabulate(m, n)(gen))
  }
}


