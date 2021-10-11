package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._


import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object Operators {

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String, delay: CyclesCount, exeTime: TimeNumber) = GeneralNode(hardware, name, delay, exeTime)
  }

  def Line[T <: Data] = DSPHardware(
    impl = (dataIns: Seq[T]) => dataIns,
    inDegree = 1,
    outWidths = Seq(-1 bits)
  )

  // for simulation(using delay)
  def sintKeep = DSPHardware((dataIns: Seq[SInt]) => Seq(dataIns(0)), 1, Seq(-1 bits))
  def sintKeep(width: BitCount) = DSPHardware((dataIns: Seq[SInt]) => Seq(dataIns(0)), 1, Seq(width))

  // for implementation
  def sIntInc(width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt]) => Seq(Delay(dataIns(0) + 1, delay.toInt, init = dataIns.head.getZero)),
    1,
    Seq(width))

  def sIntCMult(constant: Int, width: BitCount, delay: CyclesCount) = DSPHardware(
      (dataIns: Seq[SInt]) => Seq(Delay((dataIns(0) * constant).resize(dataIns(0).getBitsWidth), delay.toInt, init = dataIns.head.getZero)),
      1,
      Seq(width))

  def sIntAdder(width: BitCount, delay: CyclesCount) = DSPHardware(
      (dataIns: Seq[SInt]) => Seq(Delay(dataIns(0) + dataIns(1), delay.toInt, init = dataIns.head.getZero)),
      2,
      Seq(width))

  def bitsKeep = DSPHardware((dataIns: Seq[Bits]) => Seq(dataIns(0)), 1, Seq(-1 bits))

}
