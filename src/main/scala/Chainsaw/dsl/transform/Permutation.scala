package Chainsaw.dsl.transform

import Chainsaw.dsl.vectorspace.VectorSpace

import scala.collection.mutable
import scala.reflect.ClassTag

class Permutation[T](override val array: Array[Array[T]], val intArray: Array[Array[Int]])
                    (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T])
  extends Matrix[T](array) {}

object Permutation {

  def order2array[T](order: Array[Int])
                 (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]) = {
    val n = order.length
    val array = mutable.Seq.tabulate(n, n)((_, _) => 0)
    order.zipWithIndex.foreach { case (oi, i) => array(i)(oi) = 1 }
    val intArray = Array.tabulate(n, n)((i, j) => array(i)(j))
    val typedArray = Array.tabulate(n, n)((i, j) => if(array(i)(j) == 1) vectorSpace.field.one else vectorSpace.field.zero)
    (typedArray, intArray)
  }

  def apply[T](order: Array[Int], parallel: Int = 1, iterative: Int = 1)
           (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]): Permutation[T] = {
    val arrays = order2array(order)
    new Permutation(arrays._1, arrays._2)
  }

  def apply[T](array: Int*)
              (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]): Permutation[T] =
    apply(array.toArray, 1, 1)

}
