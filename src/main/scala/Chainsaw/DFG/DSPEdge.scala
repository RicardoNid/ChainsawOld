package Chainsaw.DFG

import spinal.core._
import spinal.lib._

abstract class DSPEdge[T] {
  val name: String
  val schedules: Seq[Schedule]
  val inOrder: Int
  val outOrder: Int
}

class DefaultDelay[T](namep: String, schedulesp: Seq[Schedule], outOrderp: Int, inOrderp: Int) extends DSPEdge[T] {
  override val name = namep
  override val schedules: Seq[Schedule] = schedulesp
  override val inOrder: Int = inOrderp
  override val outOrder: Int = outOrderp

  override def toString: String = s"$name: ${schedules.mkString(" ")}"
}

object DefaultDelay {
  def apply[T](name: String, schedulesp: Seq[Schedule], outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay(name, schedulesp, outOrder, inOrder)

  def apply[T](schedulesp: Seq[Schedule], outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay("noNameEdge", schedulesp, outOrder, inOrder)

  def apply[T](outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay("noNameEdge", Seq(Schedule(0, 1)), outOrder, inOrder)
}