package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

trait Foldable[T <: Data] {
  def fold(sources: Seq[DSPNode[T]]): DSPNode[T]
}

trait Transform {
  def latencyTrans: LatencyTrans
}
