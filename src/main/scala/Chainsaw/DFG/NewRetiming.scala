package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

class NewRetiming[T <: Data](override val dfg: DFGGraph[T], solution: Map[DSPNode[T], Int]) extends DFGTransform[T] {

  override val transformName: String = "retiming"
  override val logger: Logger = LoggerFactory.getLogger("retiming procedure")

  def r(node: DSPNode[T]): Int = solution.getOrElse(node, 0)

  override def periodTransform(period: Int): Int = period

  override def ioMultiple: Int = 1

  override def spaceTransform(iteration: Iteration[T]): DSPNode[T] = iteration.device

  override def timeTransform(iteration: Iteration[T]): Int = iteration.time + r(iteration.device)

  override def rangeInvolved: Int = dfg.period

  override def constraint(sourceIteration: Iteration[T], targetIteration: Iteration[T]): DSPConstraint[T] = {
    val (u, v) = (sourceIteration.device, targetIteration.device)
    val (tU, tV) = (sourceIteration.time, targetIteration.time)
    v - u >= tU - tV + r(u) - r(v) + u.delay
  }

  override def getTransformed: DFGGraph[T] =
    if (isValid) transformed
    else throw new IllegalArgumentException(s"invalid retiming values, constraint:${constraints.mkString(" ")}")
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

    val ret = new NewRetiming(dfg, inputRetimingValues ++ outputRetimingValues).transformed
    ret.setLatency(outputPosition - inputPosition)
    ret
  }

  //  def retimingByConstraint(constraints:Seq[DSPConstraint])
}
