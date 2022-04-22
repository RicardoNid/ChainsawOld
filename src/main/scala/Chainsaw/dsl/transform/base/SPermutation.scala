package Chainsaw.dsl.transform.base

import Chainsaw.dsl._
import spinal.core._

import scala.reflect.ClassTag


class SPermutation[T](val row: Int, val column: Int)
                     (implicit tag: ClassTag[T], field: Field[T])
  extends BaseTransform[T, T](
    SPermutation.getTransform(row, column),
    SPermutation.getImpl(row, column),
    SPermutation.getSize(row, column))(tag, tag, field, field)


object SPermutation {

  def getTransform[T](row: Int, column: Int)(implicit tag: ClassTag[T]) =
    (dataIn: Array[T]) => {
      require(dataIn.length == row * column)
      dataIn.grouped(row).toArray.transpose.flatten
    }

  // todo: implement this
  def getImpl(row: Int, column: Int) = (dataIn: Vec[Bits]) => dataIn

  def getSize(row: Int, column: Int) = (row * column, row * column)

  def apply[T](row: Int, column: Int)
              (implicit tag: ClassTag[T], field: Field[T]): SPermutation[T] =
    new SPermutation(row, column)(tag, field)

}
