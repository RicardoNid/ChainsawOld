package Chainsaw.dsl.vectorspace

import Chainsaw.dsl.field.Field

import scala.reflect.ClassTag

class BasicVectorSpace[T](implicit field: Field[T], classTag: ClassTag[T]) extends VectorSpace[T] {
  /** basic operation of Chainsaw vector space which should be implemented according to hardware architecture
   *
   * @param a matrix, the transform
   * @param b column vector, the signal
   * @return column vector
   */
  override def gemv(a: Array[Array[T]], b: Array[T]) = {

    def adderTree(array: Array[T]): Array[T] = {
      val n = array.length
      if (n == 1) array
      else {
        val (left, right) = array.splitAt(n / 2)
        val sums: Array[T] = left.zip(right).map { case (a, b) => field.add(a, b) }
        val remain: Array[T] = if (n % 2 == 1) Array(field.identity(array.last)) else Array.empty[T]
        adderTree(sums ++ remain)
      }
    }

    val afterMult: Array[Array[T]] = // constant multiplication
      a.map(row => row.zip(b).map { case (eleA, eleB) => field.multiply(eleA, eleB) })

    val ret = afterMult.map(adderTree(_).head) // adder tree
    ret
  }
}

object BasicVectorSpace {
  def apply[T](implicit field: Field[T], classTag: ClassTag[T]): BasicVectorSpace[T] = new BasicVectorSpace()
}
