package Chainsaw.dsl.vectorspace

import Chainsaw.dsl.field.Field

import scala.reflect.ClassTag

abstract class VectorSpace[T](implicit val field: Field[T], classTag: ClassTag[T]) {

  implicit val vectorSpace = this

  /** basic operation of Chainsaw vector space which should be implemented according to hardware architecture
   *
   * @param a matrix, the transform
   * @param b column vector, the signal
   * @return column vector
   */
  def gemv(a: Array[Array[T]], b: Array[T]): Array[T]

}
