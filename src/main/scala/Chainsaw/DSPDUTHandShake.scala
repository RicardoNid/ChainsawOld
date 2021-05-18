package Chainsaw

import spinal.core.Data
import spinal.lib.Stream

@unchecked // TODO: develop this in the future
trait DSPDUTHandShake[inputType <: Data, outputType <: Data] {
  val input: Stream[inputType]
  val output: Stream[outputType]
}