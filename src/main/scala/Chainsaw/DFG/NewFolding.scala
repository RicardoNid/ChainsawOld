package Chainsaw.DFG

import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

import scala.math.ceil

/** my folding algorithm based on periodic time-space transformation
 *
 */
class NewFolding[T <: Data](override val dfg: DFGGraph[T], foldingSet: Seq[Seq[DSPNode[T]]]) extends Transform[T] {

  implicit val referenceDFG: DFGGraph[T] = dfg

  val logger: Logger = LoggerFactory.getLogger("folding procedure")

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

  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSet.flatMap { nodes => // map node -> folded device of the node
    val nonEmptyNodes = nodes.filterNot(_ == null)
    nonEmptyNodes.map(_ -> nonEmptyNodes.head)
  }.toMap

  override val transformName: String = "folding"

  override def iterationsInvolved: Seq[Iteration[T]] =
    dfg.vertexSeq.flatMap(device => (0 until dfg.period).map(i => Iteration(device, i)))

  override def periodTransform(period: Int): Int = dfg.period * N

  override def timeSpaceTransform(iteration: Iteration[T]): Iteration[T] = {

    val (oldDevice, oldTime) = (iteration.device, iteration.time)
    val newDevice = deviceOf.getOrElse(oldDevice, oldDevice)
    val foldingOrder = foldingOrderOf(oldDevice)
    val newSchedule = oldTime * N + foldingOrder
    Iteration(newDevice, newSchedule)
  }

  override def constraint(sourceIteration: Iteration[T], targetIteration: Iteration[T]): DSPConstraint[T] = {
    val (u, v) = (sourceIteration.device, targetIteration.device)
    val (tU, tV) = (sourceIteration.time, targetIteration.time)
    logger.info(s"constraint $tU - $tV + floor((${u.delay} - ${foldingOrderOf(v)} + ${foldingOrderOf(u)}).toDouble / $N).toInt")
    v - u >= tU - tV + ceil((u.delay - foldingOrderOf(v) + foldingOrderOf(u)).toDouble / N).toInt
  }

}
