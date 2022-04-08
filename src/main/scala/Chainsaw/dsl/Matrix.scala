package Chainsaw.dsl

import scala.reflect.ClassTag

class Matrix[T](val array: Array[Array[T]], val parallel: Int, val iterative: Int)
               (implicit classTag: ClassTag[T]) {

  def rows = array.length

  def columns = array.head.length

  def parallelExpand(multiple: Int) = Matrix(array, parallel * multiple, iterative)

  def iterativeExpand(multiple: Int) = Matrix(array, parallel, iterative * multiple)

  def *(that: Matrix[T])(implicit vectorSpace: VectorSpace[T]) = vectorSpace.gemm(this, that)

  def *:(that: Matrix[T])(implicit vectorSpace: VectorSpace[T]) = vectorSpace.gemm(that, this)

  override def toString = {
    val widthMax = array.flatten.map(_.toString.length).max
    "matrix:\n" + array.map(_.map(_.toString.padTo(widthMax, ' ')).mkString(" ")).mkString("\n")
  }
}

object Matrix {
  def apply[T](array: Array[Array[T]], parallel: Int, iterative: Int)
              (implicit classTag: ClassTag[T]): Matrix[T] = new Matrix(array, parallel, iterative)

  def apply[T](array: Array[T])(implicit classTag: ClassTag[T]): Matrix[T] = Matrix(array.map(Array(_)), 1, 1)
}


