package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class Schedule(time: Int, period: Int) {
  override def toString: String = s"$time / $period"

  def timesUnderPeriod(globalPeriod: Int): Seq[Int] = (0 until globalPeriod / period).map(i => i * period + time)
}

object NoMUX {
  def apply() = Seq(Schedule(0, 1))
}

case class GlobalCount(value: UInt)

case class DFGMUX[T <: Data](schedules: Seq[Seq[Schedule]], period: Int)(implicit holderProvider: HolderProvider[T]) {

  def localLcm: Int = schedules.flatten.map(_.period).sorted.reverse.reduce(lcm)

  def occupationOf(schedule: Schedule): Seq[Int] = (0 until period / schedule.period).map(_ * schedule.period + schedule.time)

  // check validity while init
  def allOccupations: Seq[Int] = schedules.flatten.flatMap(occupationOf)

  def hasNoCollisions: Boolean = allOccupations.size == allOccupations.distinct.size

  def isFull: Boolean = allOccupations.distinct.size == localLcm

  def impl(dataIns: Seq[T], asROM: Boolean = false)(implicit globalCount: GlobalCount): T = {
    //    printlnGreen(s"implementing mux $this")
    require(hasNoCollisions, s"schedule collision:\n${schedules.mkString(" ")}")
    require(dataIns.size == schedules.size)
    if (dataIns.size == 1 && schedules.head.head == Schedule(0, 1)) dataIns.head
    else {
      val retWidth = dataIns.map(_.getBitsWidth).max bits
      val ret      = holderProvider(retWidth)

      if (asROM) {
        val ROMValues = ArrayBuffer.fill(period)(dataIns.head)
        dataIns.zip(schedules).foreach { case (bits, schedulesOneSource) =>
          val occupationsOneSource: Seq[Int] = schedulesOneSource.flatMap(occupationOf)
          occupationsOneSource.foreach(i => ROMValues(i) = bits)
        }
        val ROM = Mem(ROMValues)
        logger.debug(s"implementing MUX as ROM")
        ROM.setName(s"rom", weak = true)
        ret := ROM.readAsync(globalCount.value)
      } else {
        switch(globalCount.value) {
          schedules.zip(dataIns).foreach { case (schedulesOneSource, bits) =>
            val occupationsOneSource: Seq[Int] = schedulesOneSource.flatMap(occupationOf)
            occupationsOneSource.foreach(is(_)(ret := bits.resized))
          }
          if (!isFull || !isPow2(period)) {
            if (globalImplPolicy.useDontCare) default(ret.assignDontCare())
            else default(ret assignFromBits B(0, retWidth))
          }
        }
      }
      ret
    }
  }

  override def toString: String = s"${schedules.map(_.map(occupationOf).map(_.mkString(" ")).mkString(" ")).mkString(" | ")} @ $localLcm"
}
