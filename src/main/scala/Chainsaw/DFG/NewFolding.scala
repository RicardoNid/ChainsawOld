package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

import scala.math.ceil

/** my folding algorithm based on periodic time-space transformation
 *
 */
class NewFolding[T <: Data](override val dfg: DFGGraph[T], foldingSet: Seq[Seq[DSPNode[T]]]) extends DFGTransform[T] {

  implicit val referenceDFG: DFGGraph[T] = dfg

  val N: Int = foldingSet.head.length // folding factor
  require(foldingSet.forall(_.size == N), "folding set should have the same folding factor on each node")

  val foldingOrderOf: Map[DSPNode[T], Int] = {
    val ofDevice = foldingSet.flatMap { set => // map node -> folding order of the node
      set.zipWithIndex.filterNot(_._1 == null) map { case (node, i) => node -> i }
    }.toMap
    ofDevice ++
      dfg.inputNodes.map(node => node -> 0) ++
      dfg.constantNodes.map(_ -> 0).toMap ++
      dfg.outputNodes.map(node => node -> ofDevice(node.sources.head))
  }

  override val transformName: String = "folding"
  override val logger: Logger = LoggerFactory.getLogger("folding procedure")

  override def periodTransform(period: Int): Int = dfg.period * N

  override def ioMultiple: Int = 1

  override def rangeInvolved: Int = dfg.period

  override def spaceTransform(iteration: Iteration[T]): DSPNode[T] = {
    val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSet.flatMap { nodes => // map node -> folded device of the node
      val nonEmptyNodes = nodes.filterNot(_ == null)
      nonEmptyNodes.map(_ -> nonEmptyNodes.head)
    }.toMap
    deviceOf.getOrElse(iteration.device, iteration.device)
  }

  override def timeTransform(iteration: Iteration[T]): Int = iteration.time * N + foldingOrderOf(iteration.device)

  override def constraint(sourceIteration: Iteration[T], targetIteration: Iteration[T]): DSPConstraint[T] = {
    val (u, v) = (sourceIteration.device, targetIteration.device)
    val (tU, tV) = (sourceIteration.time, targetIteration.time)
    logger.debug(s"constraint $tU - $tV + floor((${u.delay} - ${foldingOrderOf(v)} + ${foldingOrderOf(u)}).toDouble / $N).toInt")
    v - u >= tU - tV + ceil((u.delay - foldingOrderOf(v) + foldingOrderOf(u)).toDouble / N).toInt
  }

  override def getTransformed: DFGGraph[T] = if (isValid) transformed else new NewFolding(retimed, foldingSet).transformed
}
