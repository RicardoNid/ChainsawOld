package Chainsaw.dsl

import scala.reflect.ClassTag

abstract class VectorSpace[T](implicit field: Field[T], classTag: ClassTag[T]) {

  implicit val vectorSpace = this

  /** basic operation of Chainsaw vector space which should be implemented according to hardware architecture
   *
   * @param a matrix, the transform
   * @param b column vector, the signal
   * @return column vector
   */
  def gemv(a: Matrix[T], b: Array[T]): Array[T]

  /**
   * @param a transform
   * @param b signal
   */
  def gemm(a: Matrix[T], b: Matrix[T]) = {
    println(s"transform size: ${a.rows} x ${a.columns}")
    println(s"signal size: ${b.rows} x ${b.columns}")
    val expandedA = a.parallelExpand(b.columns)
    val flattenedB = b.array.transpose.flatten
    val retCol = gemv(expandedA, flattenedB) // gemm implemented by reusing gemv
    Matrix(retCol.grouped(b.rows).toArray, 1, 1)
  }

  def *(a: Matrix[T], b: Matrix[T]) = gemm(a, b)
}
