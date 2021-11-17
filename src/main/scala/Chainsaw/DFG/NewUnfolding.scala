package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

class NewUnfolding[T <: Data](override val dfg: DFGGraph[T], unfoldingFactor: Int) extends DFGTransform[T] {

  override val transformName: String = "unfolding"
  override val logger: Logger = LoggerFactory.getLogger("unfolding procedure")

  val unfoldedDevicesMap: Map[DSPNode[T], Seq[DSPNode[T]]] =
    dfg.vertexSeq.map(device => device -> (0 until unfoldingFactor).map(i => device.copy(s"${device.name}_unfolded$i"))).toMap

  override def periodTransform(period: Int): Int =
    if (unfoldingFactor >= dfg.period) {
      require(unfoldingFactor % dfg.period == 0)
      1
    }
    else { // TODO: add tests for this situation
      require(dfg.period % unfoldingFactor == 0)
      dfg.period / unfoldingFactor
    }

  override def ioMultiple: Int = unfoldingFactor

  override def rangeInvolved: Int = unfoldingFactor max dfg.period

  override def spaceTransform(iteration: Iteration[T]): DSPNode[T] = unfoldedDevicesMap(iteration.device)(iteration.time % unfoldingFactor)

  override def timeTransform(iteration: Iteration[T]): Int = iteration.time / unfoldingFactor

  override def constraint(sourceIteration: Iteration[T], targetIteration: Iteration[T]): DSPConstraint[T] = sourceIteration.device - targetIteration.device <= 0 // always valid

  override def getTransformed: DFGGraph[T] = transformed
}
