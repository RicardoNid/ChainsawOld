package Chainsaw.dsl.vectorspace

import Chainsaw.dsl.field.{RingOp, MixType}
import spinal.core._

import scala.reflect.ClassTag

class BasicVectorSpace[T:ClassTag](override val field: MixType[T] with RingOp[T])
  extends VectorSpace[T](field) {
  /** basic operation of Chainsaw vector space which should be implemented according to hardware architecture
   *
   * @param a matrix, the transform
   * @param b column vector, the signal
   * @return column vector
   */
  override def gemv(a: Array[Array[T]], b: Array[Bits]) = {

    def adderTree(array: Array[Bits]): Array[Bits] = {
      val n = array.length
      if (n == 1) array
      else {
        val (left, right) = array.splitAt(n / 2)
        val sums: Array[Bits] = left.zip(right).map { case (a, b) => field.addH(a, b) }
        val remain: Array[Bits] = if (n % 2 == 1) Array(field.idH(array.last)) else Array.empty[Bits]
        adderTree(sums ++ remain)
      }
    }

    val afterMult: Array[Array[Bits]] = // constant multiplication
      a.map(row => row.zip(b).map { case (eleA, eleB) => field.multH(field.toCoeff(eleA), eleB) })

    val ret = afterMult.map(adderTree(_).head) // adder tree
    ret
  }

  override def gemv(a: Array[Array[T]], b: Array[T]) = a.map(row => row.zip(b).map { case (eleA, eleB) => field.mult(eleA, eleB) }.reduce(field.add))
}

object BasicVectorSpace {
  def apply[T](field: MixType[T] with RingOp[T])(implicit classTag: ClassTag[T]): BasicVectorSpace[T] = new BasicVectorSpace(field)
}
