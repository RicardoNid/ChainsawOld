package Chainsaw.DFG

import org.slf4j.LoggerFactory
import scala.math.floor

class Folding[T](dfg: DFGGraph[T], foldingSet: Seq[Seq[DSPNode[T] with Foldable[T]]]) extends Transform {
  val logger = LoggerFactory.getLogger("folding procedure")

  // preparing context for folding
  val N = foldingSet.head.length
  require(foldingSet.forall(_.size == N), "folding set should have the same folding factor on each node")
  // map node -> folding order of the node
  val foldingOrderOf: Map[DSPNode[T], Int] = foldingSet.flatMap { set => set.zipWithIndex.filterNot(_._1 == null) map { case (node, i) => node -> i } }.toMap
  val devices: Seq[DSPNode[T]] = foldingSet.map { nodes => // map node -> the device it belongs(folded to)
    val nonEmptyNode = nodes.filterNot(_ == null).head // TODO: better solution
    // replace null by an arbitrary nonempty node, which should have no influence on the function as result of null operation won't be used
    val filledNodes: Seq[DSPNode[T] with Foldable[T]] = nodes.map(node => if (node != null) node else nonEmptyNode)
    logger.debug(s"folding ${filledNodes.mkString(" ")}")
    logger.debug(s"folding ${filledNodes.map(_.getClass).mkString(" ")}")
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
    val u = if (U.isIO) foldingOrderOf(V) else foldingOrderOf(U) // input follows its target
    val v = if (V.isIO) foldingOrderOf(U) else foldingOrderOf(V) // output follows its source
    val w = edge.weightWithSource
    val Pu = if (U.isIO) 0 else deviceOf(U).delay
    logger.debug(s"calculating delay: $N * $w - $Pu + $v - $u = ${N * w - Pu + v - u}")
    N * w - Pu + v - u
  }

  lazy val solveRetiming: Map[DSPNode[T], Int] = {
    implicit val sourceDFG: DFGGraph[T] = dfg
    val cg = ConstraintGraph(dfg)
    logger.debug(s"original folded delays ${dfg.edgeSeq.map(foldedDelay).mkString(" ")}")
    dfg.foreachEdge { edge => // for each edge, add the folding equation constraint as extra constraint
      val U = edge.source
      val V = edge.target
      // folded delay >= 0 => N * (r(V) - r(U) + w) - Pu + v - u >= 0 => r(U) - r(V) <= (Nw + - Pu + v - u)/N
      cg.addConstraint(U - V <= floor(foldedDelay(edge).toDouble / N).toInt)
    }
    cg.getSolution
  }

  lazy val retimed = dfg.retimed(solveRetiming)

  lazy val folded = {
    logger.info("start folding")
    logger.debug(s"original:\n$dfg")
    val solution = solveRetiming
    logger.debug(s"retiming solution:\n${solution.mkString(" ")}")
    implicit val retimedDFG: DFGGraph[T] = dfg.retimed(solution)
    logger.debug(s"retimed dfg:\n$retimedDFG")
    logger.debug(s"retimed folded delays ${retimedDFG.edgeSeq.map(foldedDelay).mkString(" ")}")
    // adding vertices
    val foldedDFG = DFGGraph[T](s"${dfg.name}_folded")
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
    logger.debug(s"folded dfg:\n$foldedDFG")
    foldedDFG
  }

  override def latencyTransformations: Seq[LatencyTrans] = {
    implicit val foldedDFG: DFGGraph[T] = folded // TODO: avoid rerun this
    val inputSchedule: Int = foldedDFG.inputNodes.head.outgoingEdges.map(_.schedules.map(_.time)).flatten.min
    val outputSchedule: Int = foldedDFG.outputNodes.head.incomingEdges.head.schedules.head.time
    new Retiming(dfg, solveRetiming).latencyTransformations :+ LatencyTrans(N, outputSchedule - inputSchedule)
  }
}
