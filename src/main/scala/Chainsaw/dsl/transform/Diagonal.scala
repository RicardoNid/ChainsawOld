package Chainsaw.dsl.transform

import Chainsaw.dsl.vectorspace.VectorSpace

import scala.collection.mutable
import scala.reflect.ClassTag

class Diagonal[T](override val array: Array[Array[T]])
                 (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T])
  extends Matrix[T](array) {


}

object Diagonal {
  def apply[T](diag: Array[T], parallel: Int = 1, iterative: Int = 1)
              (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]): Diagonal[T] = {

    val n = diag.length
    val array = mutable.Seq.tabulate(n, n)((_, _) => vectorSpace.field.zero)
    diag.indices.foreach(i => array(i)(i) = diag(i))
    val immutable = Array.tabulate(n, n)((i, j) => array(i)(j))
    new Diagonal(immutable)
  }

  def apply[T](diag: T*)
              (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]): Diagonal[T] =
    apply(diag.toArray, 1, 1)(tag, vectorSpace)
}
