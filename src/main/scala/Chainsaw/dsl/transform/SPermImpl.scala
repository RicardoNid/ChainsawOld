package Chainsaw.dsl.transform

import Chainsaw.dsl._
import spinal.core._

class SPermImpl(row:Int, column:Int) extends HardImpl{

  override val size = (row * column, row * column)

  override def getImpl(spaceFold: Int, timeFold: Int) = {
    val impl = (dataIn: (Vec[Bits],Bool)) => {
      val ret = Vec(dataIn._1.toArray.grouped(row).toArray.transpose.flatten)
      (ret, dataIn._2)
    }
    RawImpl(impl, 0)
  }
}
