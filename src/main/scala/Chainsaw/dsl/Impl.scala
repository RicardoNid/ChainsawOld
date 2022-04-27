package Chainsaw.dsl

import spinal.core._
import spinal.lib._

case class RawImpl(impl: ((Vec[Bits], Bool)) => (Vec[Bits], Bool), latency: Int)

abstract class ImplComponent(inWidth: Int, outWidth: Int, inSize: Int, outSize: Int) extends Component {
  val dataIn = in(Fragment(Vec(Bits(inWidth bits), inSize)))
  val dataOut = out(Fragment(Vec(Bits(outWidth bits), outSize)))
  val latency: Int
}

abstract class Impl {

  val foldMax: Int = 1 // default value
  val size: (Int, Int) = (1, 1)

  def getImpl(fold: Int): RawImpl
  //  def getImpl(fold: Int): _ => ImplComponent
}
