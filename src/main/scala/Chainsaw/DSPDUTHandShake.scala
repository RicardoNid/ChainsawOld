package Chainsaw

import spinal.core.Data
import spinal.lib.Stream

trait DSPDUTHandShake[inputType <: Data, outputType <: Data] {
  val input: Stream[inputType]
  val output: Stream[outputType]
}