package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

case class Schedule(time: Int, period: Int) {
  override def toString: String = s"$time / $period"

  def timesUnderPeriod(globalPeriod: Int): Seq[Int] = (0 until globalPeriod / period).map(i => i * period + time)
}

object NoMUX {
  def apply() = Seq(Schedule(0, 1))
}

case class GlobalCount(value: UInt)

case class DFGMUX[T <: Data](schedules: Seq[Seq[Schedule]])
                            (implicit holderProvider: HolderProvider[T]) {

  def localLcm: Int = schedules.flatten.map(_.period).sorted.reverse.reduce(lcm)

  def occupationOf(schedule: Schedule): Seq[Int] = (0 until localLcm / schedule.period).map(_ * schedule.period + schedule.time)

  // check validity while init
  def allOccupations: Seq[Int] = schedules.flatten.flatMap(occupationOf)

  def hasNoCollisions: Boolean = allOccupations.size == allOccupations.distinct.size

  def isFull: Boolean = allOccupations.distinct.size == localLcm

  def impl(dataIns: Seq[T], globalLcm: Int)(implicit globalCount: GlobalCount): T = {
    //    printlnGreen(s"implementing mux $this")
    require(hasNoCollisions, s"schedule collision:\n${schedules.mkString(" ")}")
    require(dataIns.size == schedules.size)
    if (dataIns.size == 1 && schedules.head.head == Schedule(0, 1)) dataIns.head
    else {
      val multiple = globalLcm / localLcm
      val retWidth = dataIns.map(_.getBitsWidth).max bits
      val ret = holderProvider(retWidth)
      switch(globalCount.value) {
        schedules.zip(dataIns).foreach { case (schedulesOneSource, bits) =>
          val occupationsOneSource = schedulesOneSource.flatMap(occupationOf)
          val actualOccupations: Seq[Int] = (0 until multiple).flatMap(i => occupationsOneSource.map(_ + i * localLcm))
          actualOccupations.foreach(is(_)(ret := bits.resized))
          logger.debug(s"implementing MUX, ${occupationsOneSource.mkString(" ")} / $localLcm")
        }

        //        if (!isFull || !isPow2(globalLcm)) default(ret.assignDontCare()) // FIXME: this should work, why?
        if (!isFull || !isPow2(globalLcm)) default(ret assignFromBits B(0, retWidth))
        //        if (!isFull || !isPow2(globalLcm)) default(ret := dataIns.head.resized)
      }
      ret
    }
  }

  override def toString: String = s"${schedules.map(_.map(occupationOf).map(_.mkString(" ")).mkString(" ")).mkString(" | ")} @ $localLcm"
}

object DFGMUX {
  def main(args: Array[String]): Unit = {
    GenRTL(new Component {
      val dataIn: Vec[Bits] = in Vec(Bits(4 bits), 3)
      val count: UInt = in UInt (log2Up(24) bits)
      implicit val globalCount: GlobalCount = GlobalCount(count)
      val dataOut: Bits = out Bits (4 bits)

      val mux: DFGMUX[Bits] = DFGMUX[Bits](Seq(
        Seq(Schedule(1, 4), Schedule(2, 4)),
        Seq(Schedule(0, 8), Schedule(4, 8)),
        Seq(Schedule(3, 12), Schedule(7, 12), Schedule(11, 12))
      ))

      dataOut := mux.impl(dataIn, 24)
    }, name = "complexMUX")

    GenRTL(new Component {
      val dataIn: Bits = in Bits (4 bits)
      val count: UInt = in UInt (log2Up(24) bits)
      implicit val globalCount: GlobalCount = GlobalCount(count)
      val dataOut: Bits = out Bits (4 bits)

      val mux: DFGMUX[Bits] = DFGMUX[Bits](Seq(Seq(Schedule(1, 1))))
      dataOut := mux.impl(Seq(dataIn), 24)
    })
  }
}
