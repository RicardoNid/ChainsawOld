package Chainsaw.DFGNew

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object Operators {

  implicit class op2node[T <: Data](op: Seq[T] => T) {
    def asDSPNode(width: BitCount, name: String, delay: CyclesCount, exeTime: TimeNumber) = GeneralNode(op, width, name, delay, exeTime)

    def asDSPNode(name: String, delay: CyclesCount, exeTime: TimeNumber) = GeneralNode(op, -1 bits, name, delay, exeTime)
  }

  val SIntInc = (dataIns: Seq[SInt]) => Delay(dataIns(0) + 1, 1, init = dataIns.head.getZero)
  val SIntAdder = (dataIns: Seq[SInt]) => Delay(dataIns(0) + dataIns(1), 1, init = dataIns.head.getZero)
  val SIntPT = (dataIns: Seq[SInt]) => dataIns.head
  val SIntMult = (dataIns: Seq[SInt]) => Delay(dataIns(0), 2, init = dataIns.head.getZero) // TODO: this should be a cMult
}
