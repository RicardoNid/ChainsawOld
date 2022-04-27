package Chainsaw.dsl

import spinal.core._
import spinal.lib._

case class RawImpl(impl: ((Vec[Bits], Bool)) => (Vec[Bits], Bool), latency: Int)

abstract class ImplComponent extends Component {
  val dataIn: Fragment[Vec[Bits]]
  val dataOut: Fragment[Vec[Bits]]
  val latency: Int
}

abstract class Impl {

  val foldMax: Int = 1 // default value
  val size: (Int, Int) = (1, 1)

  def getImpl(fold: Int): RawImpl
  //  def getImpl(fold: Int): _ => ImplComponent
}
