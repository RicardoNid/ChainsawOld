package Chainsaw.dsl

import scala.reflect.ClassTag

class BasicVectorSpace[T](implicit field: Field[T], classTag: ClassTag[T]) extends VectorSpace[T] {
  /** basic operation of Chainsaw vector space which should be implemented according to hardware architecture
   *
   * @param a matrix, the transform
   * @param b column vector, the signal
   * @return column vector
   */
  override def gemv(a: Matrix[T], b: Array[T]) = {

    def basicGemv(a: Array[Array[T]], b: Array[T]) = {

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
      afterMult.map(adderTree(_).head) // adder tree

    }

    val bs: Array[Array[T]] = b.grouped(b.length / a.parallel).toArray // segments of b
    bs.map(b => // for a.parallel instances, parallely invoke gemv
      Array.iterate(b, a.iterative + 1)(basicGemv(a.array, _)).last // for a.iterative times, iteratively iovoke gemv
    ).flatten // conbine results from segments

  }
}

object BasicVectorSpace {
  def apply[T](implicit field: Field[T], classTag: ClassTag[T]): BasicVectorSpace[T] = new BasicVectorSpace()
}
