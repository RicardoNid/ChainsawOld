package Chainsaw.dsl.transform

import Chainsaw.dsl._
import spinal.core._

class ConverterImpl(sizeIn: Int, sizeOut: Int, widthOut: Int) extends HardImpl {

  override val size = (sizeIn, sizeOut)

  override def getImpl(spaceFold: Int, timeFold: Int) = {
    val impl = (dataIn: (Vec[Bits], Bool)) => {
      val ret = Vec(dataIn._1.reverse).asBits.subdivideIn(widthOut bits)
      (ret, dataIn._2)
    }
    RawImpl(impl, 0)
  }
}
