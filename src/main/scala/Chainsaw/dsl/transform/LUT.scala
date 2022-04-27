package Chainsaw.dsl.transform

import Chainsaw.dsl
import Chainsaw.dsl._
import spinal.core._
import spinal.lib._

import scala.reflect.ClassTag
import LUT._
import spinal.lib.Fragment

class LUT[T: ClassTag]
(val lut: Array[T])
(implicit mixTypeIn: MixType[Int], mixTypeOut: MixType[T])
  extends dsl.BaseTransform[Int, T](
    getAlgo(lut),
    new LutImpl(lut.map(mixTypeOut.toBits)))

object LUT {

  def getAlgo[T: ClassTag](lut: Array[T]): Algo[Int, T] =
    (dataIn: Array[Int]) => {
      require(dataIn.length == 1)
      Array(lut(dataIn.head))
    }

  def apply[T: ClassTag]
  (lut: T*)
  (implicit mixTypeIn: MixType[Int], mixTypeOut: MixType[T]): LUT[T] =
    new LUT(lut.toArray)
}

class LutImpl(lut: Array[String]) extends Impl {

  override def getImpl(fold: Int) = {
    val component = LUTModule(lut)
    val impl = (dataIn: (Vec[Bits], Bool)) => {
      component.dataIn.fragment := dataIn._1.head.asUInt
      component.dataIn.last := dataIn._2
      (Vec(component.dataOut.fragment), component.dataIn.last)
    }
    RawImpl(impl, 1)
  }
}

case class LUTModule(lut: Array[String]) extends Component {
  val dataIn = in (Fragment(UInt(log2Up(lut.length) bits)))
  val dataOut = out (Fragment(Bits(lut.head.length bits)))
  val rom = Mem(lut.map(B(_)))
  dataOut.fragment := rom.readSync(dataIn.fragment)
  dataOut.last := RegNext(dataIn.last, init = False)
}