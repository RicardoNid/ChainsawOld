package Chainsaw.dsl.transform

import Chainsaw.dsl.vectorspace.VectorSpace

import scala.reflect.ClassTag

class SPermutation[T](override val array: Array[Array[T]], override val intArray: Array[Array[Int]])
                  (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T])
  extends Permutation[T](array, intArray) {

}

object SPermutation {

  def apply[T](row: Int, col: Int, parallel: Int = 1, iterative: Int = 1)
           (implicit tag: ClassTag[T], vectorSpace: VectorSpace[T]): SPermutation[T] = {
    val order = Array.tabulate(col, row)((j,k) => k * col + j).flatten
    val arrays = Permutation.order2array(order)
    new SPermutation(arrays._1, arrays._2)
  }
}
