package Chainsaw

import spinal.core.Data
import spinal.lib.Stream
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

@unchecked // TODO: develop this in the future
abstract class DSPDUTHandShake[inputType <: Data, outputType <: Data] extends Component {
  val input: Stream[inputType]
  val output: Stream[outputType]
}