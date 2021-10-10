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
  val outputWidths = Seq(10 bits)

  val SintKeep = DSPHardware((dataIns: Seq[SInt]) => Seq(dataIns(0)), 1, outputWidths)
  val SIntInc = DSPHardware((dataIns: Seq[SInt]) => Seq(dataIns(0) + 1), 1, outputWidths)
  val SIntAdder = DSPHardware((dataIns: Seq[SInt]) => Seq(dataIns(0) + dataIns(1)), 2, outputWidths)

  val SIntAdderPipe = DSPHardware((dataIns: Seq[SInt]) => Seq(Delay(dataIns(0) + dataIns(1), 1, init = dataIns.head.getZero)), 2, outputWidths)
  val SIntPT = DSPHardware((dataIns: Seq[SInt]) => Seq(dataIns.head), 1, outputWidths)
  val SIntCMult = DSPHardware((dataIns: Seq[SInt]) => Seq(dataIns(0)), 1, outputWidths) // TODO: this should be a cMult
  val SIntCMultPipe = DSPHardware((dataIns: Seq[SInt]) => Seq(Delay(dataIns(0), 2, init = dataIns.head.getZero)), 1, outputWidths) // TODO: this should be a cMult

  // for implementation

}
