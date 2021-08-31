package Chainsaw.Transform

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

abstract class TransformCore[Tin <: Data, Tout <: Data] extends Component {

  val dataIn: Tin
  val dataOut: Tout

  val start = in(Bool())
  val last = in(Bool())
  val ready = out(RegInit(False))

  def timingInfo: TimingInfo

  // without setting name, they won't appear in the design
  start.setName("start")
  last.setName("last")
  ready.setName("ready")
}
