package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

abstract class DSPNode {
  def transform(dataIn: Bits): Bits

  def delay: Int

  def executionTime: Double
}
