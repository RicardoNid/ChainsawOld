package Chainsaw.dataflow

import scala.collection.mutable

case class RegisterAllocation(timeRange: Int, period: Int) {

  val occupation = mutable.Seq.fill(timeRange)(mutable.Seq[Int]())

  def get(time: Int, reg: Int) = {
    if (reg >= occupation(time).length) -1
    else occupation(time)(reg)
  }

  def alloc(time: Int, reg: Int, value: Int) = {
    if (occupation(time).length > reg) occupation(time)(reg) = value
    else {
      occupation(time) = occupation(time).padTo(reg + 1, -1)
      occupation(time)(reg) = value
    }
  }

  def set(time: Int, reg: Int, value: Int) = {
    val targets = (time until timeRange).filter(t => (t - time) % period == 0)
    targets.foreach(target => alloc(target, reg, value))
  }

  def registerCount = occupation.map(_.length).max

  val charChange = (index: Int) => if (index < 0) "-" else index.toString

  override def toString = {
    val head = "register allocation: \n"
    val content = occupation.map(_.map(charChange(_).padTo(2, ' ')).mkString(" "))
      .zipWithIndex.map { case (str, time) => s"time: $time".padTo(10, ' ') + s"| $str |" }.mkString("\n")
    head + content
  }
}

abstract class RegisterAllocator {

  def getAllocation: RegisterAllocation

}


class ForwardRegisterAllocator(conversion: FlowConversion) extends RegisterAllocator {

  def getAllocation = {

    val timeRange = conversion.latency + conversion.period
    val period = conversion.period
    val allocation = RegisterAllocation(timeRange, period)

    def allocForward(initTime: Int, initReg: Int, data: Int) =
      (0 until conversion.lifeCycles(data))
        .foreach(i => allocation.set(initTime + i, initReg + i, data))

    (0 until period).foreach { time =>
      val dataIns = (0 until conversion.dataCount)
        .filter(conversion.tIns(_) == time)
        .sortBy(conversion.lifeCycles(_))
      val regs = (0 until allocation.occupation(time).length + dataIns.length)
        .filter(allocation.get(time + 1, _) == -1) // available regs
        .take(dataIns.length) // first w_{in} available regs
      dataIns.zip(regs).foreach { case (data, reg) => allocForward(time + 1, reg, data) }
    }

    allocation
  }

}


















