package Chainsaw.dsl.transform

import Chainsaw.dsl._
import spinal.core._

class LutImpl(lut:Array[String]) extends HardImpl {

  override def getImpl(spaceFold: Int, timeFold: Int) = {
    val impl = (dataIn: (Vec[Bits], Bool)) => {
      val rom = Mem(lut.map(B(_)))
      val ret = Vec(rom.readAsync(dataIn._1.head.asUInt))
      (ret, dataIn._2)
    }
    RawImpl(impl, 0)
  }
}
