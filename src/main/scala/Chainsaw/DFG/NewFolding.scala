package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConversions._

/** my folding algorithm based on periodic time-space transformation
 *
 */
class NewFolding[T <: Data](val dfg: DFGGraph[T], foldingSet: Seq[Seq[DSPNode[T]]]) extends Transform[T] {

  implicit val referenceDFG = dfg

  val logger: Logger = LoggerFactory.getLogger("folding procedure")

  val N: Int = foldingSet.head.length // folding factor
  require(foldingSet.forall(_.size == N), "folding set should have the same folding factor on each node")

  val foldingOrderOf: Map[DSPNode[T], Int] = foldingSet.flatMap { set => // map node -> folding order of the node
    set.zipWithIndex.filterNot(_._1 == null) map { case (node, i) => node -> i }
  }.toMap

  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSet.flatMap { nodes => // map node -> folded device of the node
    val nonEmptyNodes = nodes.filterNot(_ == null)
    nonEmptyNodes.map(_ -> nonEmptyNodes.head)
  }.toMap

  override val transformName: String = "folding"

  override def periodTransform(period: Int): Int = dfg.period * N

  override def iterationsInvolved: Seq[Iteration[T]] = (dfg.innerNodes ++ dfg.inputNodes ++ dfg.constantNodes).flatMap(device => (0 until dfg.period).map(i => Iteration(device, i)))

  override def timeSpaceTransform(iteration: Iteration[T]): Iteration[T] = {
    val (oldDevice, oldTime) = (iteration.device, iteration.time)
    val newDevice = deviceOf.getOrElse(oldDevice, oldDevice)
    val foldingOrder = oldDevice match {
      case node: DeviceNode[_] => foldingOrderOf(oldDevice)
      case node: ConstantNode[_] => 0
      case input: InputNode[_] => 0
      case output: OutputNode[_] => foldingOrderOf(output.sources.head)
      case _ => throw new IllegalArgumentException(s"a more accurate class rather than ${oldDevice.getClass} should be specified")
    }
    val newSchedule = oldTime * N + foldingOrder
    Iteration(newDevice, newSchedule)
  }
}
