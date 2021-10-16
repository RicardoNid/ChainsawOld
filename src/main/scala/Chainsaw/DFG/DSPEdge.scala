package Chainsaw.DFG

import spinal.core._
import spinal.lib._

abstract class DSPEdge[T <: Data] {
  val name: String
  val hardware: (Seq[T], Int) => Seq[T] // the delay is from weight, which is of type double
  val schedules: Seq[Schedule]
  val inOrder: Int
  val outOrder: Int
}

class DefaultDelay[T <: Data](namep: String, schedulesp: Seq[Schedule], outOrderp: Int, inOrderp: Int) extends DSPEdge[T] {
  override val name = namep
  override val hardware = (dataIns: Seq[T], delay: Int) => dataIns.map(dataIn => Delay(dataIn, delay, init = dataIn.getZero)) // TODO: add a FIFO(free run) implementation
  override val schedules: Seq[Schedule] = schedulesp
  override val inOrder: Int = inOrderp
  override val outOrder: Int = outOrderp

  override def toString: String = s"$name: ${schedules.mkString(" ")}"
}

object DefaultDelay {
  def apply[T <: Data](name: String, schedulesp: Seq[Schedule], outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay(name, schedulesp, outOrder, inOrder)

  def apply[T <: Data](schedulesp: Seq[Schedule], outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay("noNameEdge", schedulesp, outOrder, inOrder)

  def apply[T <: Data](outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay("noNameEdge", Seq(Schedule(0, 1)), outOrder, inOrder)
}