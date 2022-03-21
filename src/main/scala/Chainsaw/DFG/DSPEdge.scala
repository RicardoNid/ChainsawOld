package Chainsaw.DFG

import spinal.core._

/** abstract superclass of all DSPEdge
  *
  * @tparam T
  *   hardware type in SpinalHDL
  */
abstract class DSPEdge[T <: Data] {
  val name: String
  val schedules: Seq[Schedule]
  val outOrder: Int
  val inOrder: Int

  def hasNoMux: Boolean = this.schedules.equals(NoMUX())

  /** get a new edge with different schedules
    */
  def changeSchedules(schedules: Seq[Schedule]): DefaultDelay[T] = DefaultDelay[T](schedules, this.outOrder, this.inOrder)

  def sameAs(that: DSPEdge[T]): Boolean =
    outOrder == that.outOrder && inOrder == that.inOrder
}

/** concrete class of DSPEdge, representing "delays" in the DFG
  *
  * @param schedules
  *   time steps at which data at this edge should be valid for target node
  * @param outOrder
  *   source node output port index
  * @param inOrder
  *   target node input port index
  * @tparam T
  *   hardware signal type in SpinalHDL
  */
class DefaultDelay[T <: Data](override val name: String, override val schedules: Seq[Schedule], override val outOrder: Int, override val inOrder: Int)
    extends DSPEdge[T] {
  override def toString: String = s"$name: ${schedules.mkString(" ")}"
}

object DefaultDelay {
  def apply[T <: Data](name: String, schedules: Seq[Schedule], outOrder: Int, inOrder: Int): DefaultDelay[T] =
    new DefaultDelay(name, schedules, outOrder, inOrder)

  def apply[T <: Data](schedules: Seq[Schedule], outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay("noNameEdge", schedules, outOrder, inOrder)

  def apply[T <: Data](outOrder: Int, inOrder: Int): DefaultDelay[T] = new DefaultDelay("noNameEdge", NoMUX(), outOrder, inOrder)
}

/** this kind of edges won't be implemented, instead, we use it to record the influence on latency through DFG transformations
  *
  * @tparam T
  */
class LatencyEdge[T <: Data] extends DSPEdge[T]() {
  override val name: String             = "latencyEdge"
  override val schedules: Seq[Schedule] = NoMUX()
  override val outOrder: Int            = 0
  override val inOrder: Int             = 0
}

object LatencyEdge {
  def apply[T <: Data](): LatencyEdge[T] = new LatencyEdge()
}
