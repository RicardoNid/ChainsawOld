package Chainsaw.DFG

import Chainsaw._

import scala.collection.JavaConversions._
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

  println(s"original:\n $dfg")
  val N = foldingSets.head.length
  require(foldingSets.forall(_.size == N))
  // node -> folding order of the node
  val foldingOrders: Map[DSPNode[T], Int] = foldingSets.map { set => set.zipWithIndex.map { case (node, i) => node -> i } }.flatten.toMap
  // node -> the device it belongs(folded to)
  val devices: Seq[DSPNode[T]] = foldingSets.map(nodes => nodes.head.fold(nodes.filterNot(_ == null)))
  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSets.zip(devices).map { case (nodes, device) => nodes.map(_ -> device) }.flatten.toMap

  def solveRetiming() = {
    val cg = ConstraintGraph[T](dfg.holderProvider) // basic constraint
    dfg.vertexSeq.filterNot(_.isIO).foreach(cg.addVertex(_))

    implicit val sourceDFG = dfg
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
      case Failure(exception) =>
        printlnRed(s"no solution for given folding set:\n${foldingSets.map(_.mkString(" ")).mkString("\n")}")
        Seq[Int]()
      case Success(value) => value.map(_.toInt)
    }
  }

  def retimed = dfg.clone().asInstanceOf[DFGGraph[T]].retimed(solveRetiming())

  def folded(implicit holderProvider: BitCount => T) = {
    val retimedDFG = retimed

    // adding vertices
    val foldedDFG = DFGGraph[T]
    (retimedDFG.inputNodes ++ retimedDFG.outputNodes).foreach(foldedDFG.addVertex(_))
    devices.foreach(foldedDFG.addVertex(_))
    // adding edges
    implicit val sourceDFG = retimedDFG
    retimedDFG.foreachEdge { edge =>
      val Uu = edge.source
      val Vv = edge.target
      val U = if (Uu.isIO) Uu else deviceOf(Uu)
      val V = if (Vv.isIO) Vv else deviceOf(Vv)
      val u = if (Uu.isIO) foldingOrders(Uu.targets.head) else foldingOrders(Uu)
      val v = if (Vv.isIO) foldingOrders(Vv.sources.head) else foldingOrders(Vv)

      val foldedDelay = if (Uu.isIO || Vv.isIO) 0 else N * edge.weightWithSource - U.delay + v - u
      assert(foldedDelay >= 0, s"folding constraints not met, delay of $Uu -> $Vv is ${edge.weight}, folded to  $foldedDelay")

      val foldedSchedules =  edge.schedules.map{ schedule => // new delay
        val newPeriod = schedule.period * N
        val newTime = schedule.time * N + v + (if(Vv.isIO) U.delay else 0) % newPeriod
        Schedule(newTime, newPeriod)
      }
      val foldedEdge = DefaultDelay[T](foldedSchedules, edge.outOrder, edge.inOrder) // new schedule
      foldedDFG.addEdge(U, V, foldedEdge)
      foldedDFG.setEdgeWeight(foldedEdge, foldedDelay)
    }
    foldedDFG
  }
}
