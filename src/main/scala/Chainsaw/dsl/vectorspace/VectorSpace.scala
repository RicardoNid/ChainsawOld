package Chainsaw.dsl.vectorspace

import spinal.core._

import scala.reflect.ClassTag
import Chainsaw.dsl._

abstract class VectorSpace[T](val field: Field[T]) {

  implicit val vectorSpace = this

  /** basic operation of Chainsaw vector space which should be implemented according to hardware architecture
   *
   * @param a matrix, the transform
   * @param b column vector, the signal
   * @return column vector
   */
  def gemv(a: Array[Array[T]], b: Array[T]): Array[T]

  def gemv(a: Array[Array[T]], b: Array[Bits]): Array[Bits]

}
