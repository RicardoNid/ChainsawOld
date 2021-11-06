package Chainsaw.DFG

import spinal.core._

abstract class DSPEdge[T <: Data] {
  val name: String
  val schedules: Seq[Schedule]
  val inOrder: Int
  val outOrder: Int

  def hasNoMux: Boolean = this.schedules.equals(NoMUX())

  /** Get a new edge with different schedules
   */
  def changeSchedules(schedules: Seq[Schedule]): DefaultDelay[T] = DefaultDelay[T](schedules, this.outOrder, this.inOrder)
}

class DefaultDelay[T <: Data](namep: String, schedulesp: Seq[Schedule], outOrderp: Int, inOrderp: Int) extends DSPEdge[T] {
  override val name = namep
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