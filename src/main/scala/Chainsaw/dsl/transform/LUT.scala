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
      component.dataIn.fragment.head := dataIn._1.head
      component.dataIn.last := dataIn._2
      (Vec(component.dataOut.fragment.head), component.dataIn.last)
    }
    RawImpl(impl, 1)
  }
}

case class LUTModule(lut: Array[String]) extends ImplComponent(
  log2Up(lut.length), lut.head.length, 1, 1) {
  val ROM = Mem(lut.map(B(_)))
  val ret = ROM.readSync(dataIn.fragment.head.asUInt)
  dataOut.fragment.head := ret
  dataOut.last := RegNext(dataIn.last, init = False)
  override val latency = 1
}