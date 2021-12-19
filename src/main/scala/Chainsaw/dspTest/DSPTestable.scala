package Chainsaw.dspTest

import spinal.core._
import spinal.lib._

trait DSPTestable[Di <: Data, Do <: Data] {
  val dataIn: DataCarrier[Di]
  val dataOut: DataCarrier[Do]
  val latency: Int
}
