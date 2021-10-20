package Chainsaw.DFG

import org.slf4j.LoggerFactory
import spinal.core._

import scala.math.floor
import scala.util.{Failure, Success, Try}

class Folding[T <: Data](dfg: DFGGraph[T], foldingSets: Seq[Seq[DSPNode[T] with Foldable[T]]]) {

  val logger = LoggerFactory.getLogger(classOf[Folding[T]])

  val N = foldingSets.head.length
  require(foldingSets.forall(_.size == N), "folding set should have the same folding factor")
  // node -> folding order of the node
  val foldingOrders: Map[DSPNode[T], Int] = foldingSets.flatMap { set => set.zipWithIndex.map { case (node, i) => node -> i } }.toMap
  // node -> the device it belongs(folded to)
  val devices: Seq[DSPNode[T]] = foldingSets.map(nodes => nodes.head.fold(nodes.filterNot(_ == null)))
  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSets.zip(devices).flatMap { case (nodes, device) => nodes.map(_ -> device) }.toMap

  def foldedDelay(edge: DSPEdge[T])(implicit dfg: DFGGraph[T]) = {
    val U = edge.source
    val V = edge.target
    val u = foldingOrders(U)
    val v = foldingOrders(V)
    if (U.isIO || U.isConstant || V.isIO) 0 else N * edge.weightWithSource - deviceOf(U).delay + v - u
  }

  def solveRetiming(): Seq[Int] = {
    val cg = ConstraintGraph(dfg)
    implicit val sourceDFG = dfg
    dfg.foreachInnerEdge{edge => println(s"original delay: ${foldedDelay(edge)}")}

    dfg.foreachInnerEdge { edge => // for each edge, add the folding equation constraint as extra constraint
      val Uu = edge.source
      val Vv = edge.target
      val u = foldingOrders(Uu)
      val v = foldingOrders(Vv)
      val w = edge.weightWithSource
      cg.addConstraint(Uu - Vv <= (w + floor((v - u - deviceOf(Uu).delay).toDouble / N)).toInt)
    }

    val solutions = Try(cg.getSolution)
    solutions match {
      case Failure(exception) => logger.error(s"no solution for given folding set:\n${foldingSets.map(_.mkString(" ")).mkString("\n")}")
        Seq[Int]()
      case Success(value) => value.map(_.toInt)
    }
  }

  def retimed = {
    val solution = solveRetiming()
    logger.info(s"retiming solution:\n${solution.mkString(" ")}")
    dfg.clone().asInstanceOf[DFGGraph[T]].retimed(solution)
  }

  def folded = {
    logger.info(s"original:\n$dfg")
    val retimedDFG = retimed
    logger.info(s"retimed dfg:\n$retimedDFG")
    // adding vertices
    val foldedDFG = DFGGraph[T]()
    (retimedDFG.inputNodes ++ retimedDFG.outputNodes ++ retimedDFG.constantNodes).foreach(foldedDFG.addVertex(_))
    logger.info(s"constant nodes: ${retimedDFG.constantNodes.mkString(" ")}")
    devices.foreach(foldedDFG.addVertex(_))
    // adding edges
    implicit val sourceDFG = retimedDFG
    retimedDFG.foreachEdge { edge =>
      val Uu = edge.source
      val Vv = edge.target
      val U = if (Uu.isIO || Uu.isConstant) Uu else deviceOf(Uu)
      val V = if (Vv.isIO) Vv else deviceOf(Vv)
      val u = if (Uu.isIO || Uu.isConstant) foldingOrders(Uu.targets.head) else foldingOrders(Uu)
      val v = if (Vv.isIO) foldingOrders(Vv.sources.head) else foldingOrders(Vv)

      val foldedDelay = if (Uu.isIO || Uu.isConstant || Vv.isIO) 0 else N * edge.weightWithSource - U.delay + v - u
      if (foldedDelay != 0) logger.warn(s"$N * ${edge.weightWithSource} - ${U.delay} + $v - $u = $foldedDelay")
      assert(foldedDelay >= 0, s"folding constraints not met, delay of $Uu -> $Vv is ${edge.weight}, folded to  $foldedDelay")

      val foldedSchedules = edge.schedules.map { schedule => // new delay
        val newPeriod = schedule.period * N
        val newTime = (schedule.time * N + v + (if (Vv.isIO) U.delay else 0)) % newPeriod
        Schedule(newTime, newPeriod)
      }
      val foldedEdge = DefaultDelay[T](foldedSchedules, edge.outOrder, edge.inOrder) // new schedule
      foldedDFG.addEdge(U, V, foldedEdge)
      foldedDFG.setEdgeWeight(foldedEdge, foldedDelay)
    }
    logger.info(s"folded dfg:\n$foldedDFG")
    foldedDFG
  }
}
