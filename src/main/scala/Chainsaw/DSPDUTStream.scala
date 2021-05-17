package Chainsaw

import spinal.core.{Data, _}
import spinal.lib.{Flow, _}

trait DSPDUTStream[inputType <: Data, outputType <: Data] {
  val input: Stream[inputType]
  val output: Stream[outputType]
  val timing: TimingInfo

  def doTiming() = {
    output.valid := Delay(input.fire, timing.latency, init = False)
  }
}
