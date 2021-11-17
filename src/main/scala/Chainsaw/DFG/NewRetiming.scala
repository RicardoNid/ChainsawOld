package Chainsaw.DFG

import spinal.core._

class NewRetiming[T <: Data](override val dfg: DFGGraph[T], solution: Map[DSPNode[T], Int]) extends Transform[T] {

  override val transformName: String = "retiming"

  def r(node: DSPNode[T]) = solution.getOrElse(node, 0)

  override def timeSpaceTransform(iteration: Iteration[T]): Iteration[T] = {
    val newDevice = iteration.device
    val newTime = iteration.time + solution.getOrElse(iteration.device, 0)
    Iteration(newDevice, newTime)
  }

  override def periodTransform(period: Int): Int = period

  override def iterationsInvolved: Seq[Iteration[T]] =
    dfg.vertexSeq.flatMap(device => (0 until dfg.period).map(i => Iteration(device, i)))

  override def constraint(sourceIteration: Iteration[T], targetIteration: Iteration[T]): DSPConstraint[T] = {
    val (u, v) = (sourceIteration.device, targetIteration.device)
    val (tU, tV) = (sourceIteration.time, targetIteration.time)
    v - u >= tU - tV + r(u) - r(v) + u.delay
  }
}

object NewRetiming {

  /** align IO by padding
   *
   * @param dfg
   * @tparam T
   */
  def alignIO[T <: Data](dfg: DFGGraph[T]): DFGGraph[T] = {
    implicit val refDFG: DFGGraph[T] = dfg

    val inputPosition = dfg.inputLatencies.min
    val outputPosition = dfg.outputLatencies.max

    val inputRetimingValues = dfg.inputNodes.zip(dfg.inputLatencies)
      .map { case (input, position) => input -> (inputPosition - position) }
      .toMap
    val outputRetimingValues = dfg.outputNodes.zip(dfg.outputLatencies)
      .map { case (input, position) => input -> (outputPosition - position) }
      .toMap

    val ret = new NewRetiming(dfg, inputRetimingValues++ outputRetimingValues).build
    ret.setLatency(outputPosition - inputPosition)
    ret
  }

  //  def retimingByConstraint(constraints:Seq[DSPConstraint])
}
