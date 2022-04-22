package Chainsaw.dsl.vectorspace

import Chainsaw.dsl.field.Field
import spinal.core.Data
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.reflect.ClassTag

class BasicVectorSpace[T](override val field: Field[T])(implicit classTag: ClassTag[T])
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
        val remain: Array[Bits] = if (n % 2 == 1) Array(field.identityH(array.last)) else Array.empty[Bits]
        adderTree(sums ++ remain)
      }
    }

    val afterMult: Array[Array[Bits]] = // constant multiplication
      a.map(row => row.zip(b).map { case (eleA, eleB) => field.multiplyH(field.toHard(eleA), eleB) })

    val ret = afterMult.map(adderTree(_).head) // adder tree
    ret
  }

  override def gemv(a: Array[Array[T]], b: Array[T]) = a.map(row => row.zip(b).map { case (eleA, eleB) => field.multiply(eleA, eleB) }.reduce(field.add))
}

object BasicVectorSpace {
  def apply[T](field: Field[T])(implicit classTag: ClassTag[T]): BasicVectorSpace[T] = new BasicVectorSpace(field)
}
