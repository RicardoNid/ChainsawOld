package Chainsaw

import spinal.core.Data
import spinal.lib.Flow
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw.Real
import Chainsaw._

trait DSPDUT[inputType <: Data, outputType <: Data] {
  val input: inputType
  val output: outputType
  val timing: TimingInfo
}
