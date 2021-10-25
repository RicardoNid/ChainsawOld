package Chainsaw.dspTest

import spinal.core._
import spinal.lib._

trait DSPTestable[Di <: Data, Do <: Data] {
  val dataIn: Flow[Di]
  val dataOut: Flow[Do]
  val latency: Int
}
