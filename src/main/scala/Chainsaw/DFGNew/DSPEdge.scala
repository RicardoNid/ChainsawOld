package Chainsaw.DFGNew

import spinal.core._
import spinal.lib._

abstract class DSPEdge[T <: Data] {
  val impl: (T, Double) => T // the delay is from weight, which is of type double
  val schedules: Seq[Schedule]
  val order: Int
}

class DefaultDelay[T <: Data](name: String, schedulesp: Seq[Schedule], orderp: Int) extends DSPEdge[T] {
  override val impl = (dataIns: T, delay: Double) => Delay(dataIns, delay.toInt)
  override val schedules: Seq[Schedule] = schedulesp
  override val order: Int = orderp

  override def toString: String = name
}

object DefaultDelay {
  def apply[T <: Data](name: String, schedulesp: Seq[Schedule], orderp: Int): DefaultDelay[T] = new DefaultDelay(name, schedulesp, orderp)
}



