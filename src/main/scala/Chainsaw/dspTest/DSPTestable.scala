package Chainsaw.dspTest

import spinal.core._
import spinal.lib._

trait DSPTestable[Di <: Data, Do <: Data] {
  val dataIn: DataCarrier[Di]
  val dataOut: DataCarrier[Do]
  val latency: Int
}

trait DSPTestableVec[Di <: Data, Do <: Data] {
  val dataIn: DataCarrier[Vec[Di]]
  val dataOut: DataCarrier[Vec[Do]]
  val latency: Int
}
