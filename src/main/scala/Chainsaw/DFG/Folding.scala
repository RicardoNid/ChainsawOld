package Chainsaw.DFG

import org.slf4j.LoggerFactory
import spinal.core._

import scala.math.floor
import scala.util.{Failure, Success, Try}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._


class Folding[T <: Data](dfg: DFGGraph[T], foldingSets: Seq[Seq[DSPNode[T] with Foldable[T]]]) {

  val logger = LoggerFactory.getLogger(classOf[Folding[T]])

  val N = foldingSets.head.length
  require(foldingSets.forall(_.size == N), "folding set should have the same folding factor")
  // node -> folding order of the node
  val foldingOrderOf: Map[DSPNode[T], Int] = foldingSets.flatMap { set => set.zipWithIndex.filterNot(_._1 == null) map { case (node, i) => node -> i } }.toMap
  // node -> the device it belongs(folded to)
  val devices: Seq[DSPNode[T]] = foldingSets.map { nodes =>
    val nonEmptyNode = nodes.filterNot(_ == null).head
    val filledNodes = nodes.map(node => if (node != null) node else nonEmptyNode) // replace null by an arbitrary nonempty node
    filledNodes.head.fold(filledNodes)
  }
  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSets.zip(devices).flatMap { case (nodes, device) => nodes.map(_ -> device) }.toMap
  printlnGreen(deviceOf.mkString(" "))

  /** Mapping delays to folded delays
   *
   * @param edge
   * @param dfg the source dfg
   * @return
   */
  def foldedDelay(edge: DSPEdge[T])(implicit dfg: DFGGraph[T]) = {
    val U = edge.source
    val V = edge.target
    // TODO: analyse this
    val u = if (U.isIO) foldingOrderOf(V) else foldingOrderOf(U)
    val v = if (V.isIO) foldingOrderOf(U) else foldingOrderOf(V)
    val w = edge.weightWithSource
    val Pu = if (U.isIO) 0 else deviceOf(U).delay
    //    logger.info(s"calculating delay: $N * $w - $Pu + $v - $u = ${N * w - Pu + v - u}")
    N * w - Pu + v - u
  }

  def solveRetiming(): Map[DSPNode[T], Int] = {
    implicit val sourceDFG = dfg
    val cg = ConstraintGraph(dfg)
    logger.info(s"original folded delays ${dfg.edgeSeq.map(foldedDelay).mkString(" ")}")
    dfg.foreachEdge { edge => // for each edge, add the folding equation constraint as extra constraint
      val U = edge.source
      val V = edge.target
      // folded delay >= 0 => N * (r(V) - r(U) + w) - Pu + v - u >= 0 => r(U) - r(V) <= (Nw + - Pu + v - u)/N
      cg.addConstraint(U - V <= floor(foldedDelay(edge).toDouble / N).toInt)
    }
    val ret = cg.getSolution
    logger.info(s"retiming solution:\n${ret.mkString(" ")}")
    ret
  }

  def retimed = dfg.clone().asInstanceOf[DFGGraph[T]].retimed(solveRetiming())

  def folded = {
    logger.info(s"original:\n$dfg")
    implicit val retimedDFG = retimed
    logger.info(s"retimed dfg:\n$retimedDFG")
    logger.info(s"retimed folded delays ${dfg.edgeSeq.map(foldedDelay).mkString(" ")}")
    // adding vertices
    val foldedDFG = DFGGraph[T]()
    (retimedDFG.inputNodes ++ retimedDFG.outputNodes ++ retimedDFG.constantNodes).foreach(foldedDFG.addVertex(_))
    devices.foreach(foldedDFG.addVertex(_))
    // adding edges
    val solution = solveRetiming()
    retimedDFG.foreachEdge { edge =>
      val V = edge.target
      val U = edge.source
      val Hu = if (U.isIO) U else deviceOf(U)
      val Hv = if (V.isIO) V else deviceOf(V)
      val v = if (V.isIO) foldingOrderOf(edge.source) else foldingOrderOf(V)
      val foldedDelay = this.foldedDelay(edge)
      val foldedSchedules = edge.schedules.map { schedule => // new delay
        val newPeriod = schedule.period * N
        val newTime = ((schedule.time + solution(V)) * N + v) % newPeriod // TODO: "solution" part should be implemented by retiming
        Schedule(newTime, newPeriod)
      }
      foldedDFG.addEdge(Hu(edge.outOrder), Hv(edge.inOrder), foldedDelay, foldedSchedules)
    }
    logger.info(s"folded dfg:\n$foldedDFG")
    foldedDFG
  }
}
