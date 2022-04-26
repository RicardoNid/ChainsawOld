package Chainsaw.dsl.transform

import Chainsaw.dsl
import Chainsaw.dsl._
import spinal.core._

import scala.reflect.ClassTag


class SPerm[T](val row: Int, val column: Int)
              (implicit tag: ClassTag[T], field: MixType[T])
  extends dsl.BaseTransform[T, T](SPerm.getTransform(row, column), new SPermImpl(row, column))(field, field)

object SPerm {

  def getTransform[T: ClassTag](row: Int, column: Int): Algo[T, T] =
    (dataIn: Array[T]) => {
      require(dataIn.length == row * column)
      dataIn.grouped(row).toArray.transpose.flatten
    }

  def apply[T](row: Int, column: Int)
              (implicit tag: ClassTag[T], field: MixType[T]): SPerm[T] =
    new SPerm(row, column)(tag, field)

}

class SPermImpl(row:Int, column:Int) extends Impl{

  override val size = (row * column, row * column)

  override def getImpl(fold:Int) = {
    val impl = (dataIn: (Vec[Bits],Bool)) => {
      val ret = Vec(dataIn._1.toArray.grouped(row).toArray.transpose.flatten)
      (ret, dataIn._2)
    }
    RawImpl(impl, 0)
  }
}
