package Chainsaw.dsl

import spinal.core._

case class RawImpl(impl: ((Vec[Bits], Bool)) => (Vec[Bits], Bool), latency: Int)

abstract class Impl {

  val foldMax: Int = 1 // default value
  val size: (Int, Int) = (1, 1)
  def getImpl(fold: Int): RawImpl
}
