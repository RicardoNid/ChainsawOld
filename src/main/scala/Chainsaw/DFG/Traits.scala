package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

trait Foldable {
  def fold[T <: Data](sources: Seq[DSPHardware[T]]): DSPHardware[T]
}
