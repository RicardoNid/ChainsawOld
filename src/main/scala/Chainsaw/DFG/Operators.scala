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
    impl = (dataIns: Seq[T], globalCount: GlobalCount) => dataIns,
    inDegree = 1,
    outWidths = Seq(-1 bits)
  )

  def sIntInc(width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay(dataIns(0) + 1, delay.toInt, init = dataIns.head.getZero)),
    1,
    Seq(width))

  // binary adder
  def sIntAdder(width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay(dataIns(0) + dataIns(1), delay.toInt, init = dataIns.head.getZero)),
    2,
    Seq(width))

  class SIntAdder(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber)
    extends GeneralNode(sIntAdder(width, delay), name, delay, exeTime) with Foldable[SInt]{
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = SIntAdder(s"foldFrom${sources.head.name}", width, delay, exeTime)
  }

  object SIntAdder {
    def apply(name: String, width: BitCount, delay: CyclesCount, exeTime: TimeNumber): SIntAdder = new SIntAdder(name, width, delay, exeTime)
  }

  // constant multiplier
  def sIntCMult(constant: Int, width: BitCount, delay: CyclesCount) = DSPHardware(
    (dataIns: Seq[SInt], _: GlobalCount) => Seq(Delay((dataIns(0) * constant).resize(dataIns(0).getBitsWidth), delay.toInt, init = dataIns.head.getZero)),
    1,
    Seq(width))

  def sintCoeffROM(constants: Seq[Int], width: BitCount, globalCount: GlobalCount) = {
    val ROM = Mem(constants.map(S(_, width)))
    ROM.readAsync(globalCount.value)
  }

  class SIntCMult(name: String, val constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber) extends
    GeneralNode(sIntCMult(constant, width, delay), name, delay, exeTime) with Foldable[SInt] {
    override def fold(sources: Seq[DSPNode[SInt]]): DSPNode[SInt] = {
      val constants = sources.map(_.asInstanceOf[SIntCMult].constant)
      val foldedFunction = (dataIns: Seq[SInt], globalCount: GlobalCount) =>
        Seq(Delay((dataIns(0) * sintCoeffROM(constants, width, globalCount)).resize(dataIns(0).getBitsWidth), delay.toInt, init = dataIns.head.getZero))
      val foldedHardware = DSPHardware(foldedFunction, 1, Seq(width))
      GeneralNode(foldedHardware, s"foldFrom${sources.head.name}", delay, exeTime)
    }
  }

  object SIntCMult {
    def apply(name: String, constant: Int, width: BitCount, delay: CyclesCount, exeTime: TimeNumber): SIntCMult = new SIntCMult(name, constant, width, delay, exeTime)
  }

  // for simulation(using delay)
  def sintKeep = DSPHardware((dataIns: Seq[SInt], _: GlobalCount) => Seq(dataIns(0)), 1, Seq(-1 bits))

  def sintKeep(width: BitCount) = DSPHardware((dataIns: Seq[SInt], _: GlobalCount) => Seq(dataIns(0)), 1, Seq(width))

  def bitsKeep = DSPHardware((dataIns: Seq[Bits], _: GlobalCount) => Seq(dataIns(0)), 1, Seq(-1 bits))

}
