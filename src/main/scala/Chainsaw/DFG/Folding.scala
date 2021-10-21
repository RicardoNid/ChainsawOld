package Chainsaw.DFG

import org.slf4j.LoggerFactory
import spinal.core._
import scala.math.floor

class Folding[T <: Data](dfg: DFGGraph[T], foldingSet: Seq[Seq[DSPNode[T] with Foldable[T]]]) extends Transform {

  val logger = LoggerFactory.getLogger(classOf[Folding[T]])

  val N = foldingSet.head.length
  require(foldingSet.forall(_.size == N), "folding set should have the same folding factor")
  // node -> folding order of the node
  val foldingOrderOf: Map[DSPNode[T], Int] = foldingSet.flatMap { set => set.zipWithIndex.filterNot(_._1 == null) map { case (node, i) => node -> i } }.toMap
  // node -> the device it belongs(folded to)
  val devices: Seq[DSPNode[T]] = foldingSet.map { nodes =>
    val nonEmptyNode = nodes.filterNot(_ == null).head // TODO: better solution
    val filledNodes = nodes.map(node => if (node != null) node else nonEmptyNode) // replace null by an arbitrary nonempty node
    filledNodes.head.fold(filledNodes)
  }
  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSet.zip(devices).flatMap { case (nodes, device) => nodes.map(_ -> device) }.toMap

  /** Mapping delays to folded delays
   *
   * @param edge an edge in the source dfg
   * @param dfg  the source dfg
   */
  def foldedDelay(edge: DSPEdge[T])(implicit dfg: DFGGraph[T]): Int = {
    val U = edge.source
    val V = edge.target
    val u = if (U.isIO) foldingOrderOf(V) else foldingOrderOf(U)
    val v = if (V.isIO) foldingOrderOf(U) else foldingOrderOf(V)
    val w = edge.weightWithSource
    val Pu = if (U.isIO) 0 else deviceOf(U).delay
    //    logger.info(s"calculating delay: $N * $w - $Pu + $v - $u = ${N * w - Pu + v - u}")
    N * w - Pu + v - u
  }

  def solveRetiming(): Map[DSPNode[T], Int] = {
    implicit val sourceDFG: DFGGraph[T] = dfg
    val cg = ConstraintGraph(dfg)
    logger.info(s"original folded delays ${dfg.edgeSeq.map(foldedDelay).mkString(" ")}")
    dfg.foreachEdge { edge => // for each edge, add the folding equation constraint as extra constraint
      val U = edge.source
      val V = edge.target
      // folded delay >= 0 => N * (r(V) - r(U) + w) - Pu + v - u >= 0 => r(U) - r(V) <= (Nw + - Pu + v - u)/N
      cg.addConstraint(U - V <= floor(foldedDelay(edge).toDouble / N).toInt)
    }
    cg.getSolution
  }

  def retimed = dfg.retimed(solveRetiming())

  def folded = {
    logger.info(s"original:\n$dfg")
    val solution = solveRetiming()
    logger.info(s"retiming solution:\n${solution.mkString(" ")}")
    implicit val retimedDFG: DFGGraph[T] = dfg.retimed(solution)
    logger.info(s"retimed dfg:\n$retimedDFG")
    logger.info(s"retimed folded delays ${retimedDFG.edgeSeq.map(foldedDelay).mkString(" ")}")
    // adding vertices
    val foldedDFG = DFGGraph[T]()
    (retimedDFG.inputNodes ++ retimedDFG.outputNodes ++ retimedDFG.constantNodes).foreach(foldedDFG.addVertex(_))
    devices.foreach(foldedDFG.addVertex(_))

    retimedDFG.foreachEdge { edge =>
      val V = edge.target
      val U = edge.source
      val Hu = if (U.isIO) U else deviceOf(U)
      val Hv = if (V.isIO) V else deviceOf(V)
      val v = if (V.isIO) foldingOrderOf(edge.source) else foldingOrderOf(V)
      val foldedDelay = this.foldedDelay(edge)
      val foldedSchedules = edge.schedules.map { schedule => // new delay
        val newPeriod = schedule.period * N
        val newTime = (schedule.time * N + v) % newPeriod
        Schedule(newTime, newPeriod)
      }
      foldedDFG.addEdge(Hu(edge.outOrder), Hv(edge.inOrder), foldedDelay, foldedSchedules)
    }
    logger.info(s"folded dfg:\n$foldedDFG")
    foldedDFG
  }

  override def latencyTrans: LatencyTrans = {
    implicit val sourceDFG: DFGGraph[T] = folded // TODO: avoid rerun this
    val inputSchedule: Int = folded.inputNodes.head.outgoingEdges.map(_.schedules.map(_.time)).flatten.min
    val outputSchedule: Int = folded.outputNodes.head.incomingEdges.head.schedules.head.time
    new Retiming(dfg, solveRetiming()).latencyTrans +LatencyTrans(N, outputSchedule - inputSchedule)
  }
}
