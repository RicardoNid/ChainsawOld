package Chainsaw

import spinal.core.Data
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw.Real
import Chainsaw._

abstract class DSPDUTTiming[inputType <: Data, outputType <: Data] extends Component {
  val input: inputType
  val output: outputType
  val timing: TimingInfo
}

