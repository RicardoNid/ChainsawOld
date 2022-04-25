package Chainsaw.dsl

import spinal.core._

case class RawImpl(impl: ((Vec[Bits], Bool)) => (Vec[Bits], Bool), latency: Int)

abstract class HardImpl {

  val spaceFold: Array[Int] = Array(1) // default value
  val timeFold: Array[Int] = Array(1)
  val size: (Int, Int) = (1,1)

  def getImpl(spaceFold: Int, timeFold: Int): RawImpl
}
