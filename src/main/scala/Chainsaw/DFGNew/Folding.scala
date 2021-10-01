package Chainsaw.DFGNew

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

class Folding[T <: Data](dfg: DFG[T], foldingSets: Seq[Seq[DSPNode[T]]], deviceGens: Seq[() => DSPNode[T]]) {

  val foldingFactor = foldingSets.head.length
  require(foldingSets.forall(_.size == foldingFactor))
  // node -> folding order of the node
  val foldingOrders: Map[DSPNode[T], Int] = foldingSets.map { set => set.zipWithIndex.map { case (node, i) => node -> i } }.flatten.toMap
  // node -> the device it belongs(folded to)
  printlnBlue(deviceGens.mkString(" "))
  val devices: Seq[DSPNode[T]] = deviceGens.map(gen => gen())
  printlnBlue(devices.mkString(" "))
  val deviceOf: Map[DSPNode[T], DSPNode[T]] = foldingSets.zip(devices).map { case (nodes, device) => nodes.map(_ -> device) }.flatten.toMap

  def solveRetiming() = {
    val cg = dfg.fcg // basic constraint
    dfg.foreachInnerEdge { edge => // for each edge, add the folding equation constraint as extra constraint
      val U = dfg.getEdgeSource(edge)
      val V = dfg.getEdgeTarget(edge)

      val u = foldingOrders(U)
      val v = foldingOrders(V)
      val w = dfg.getEdgeWeight(edge)
      //      println(s"adding constraint $U - $V <= $w + floor(${(v - u - U.delay).toDouble} / $foldingFactor)")
      cg.addConstraint(U - V <= (w + floor((v - u - U.delay).toDouble / foldingFactor)).toInt) // folding constraint, Nw_r(e) - P_u + v - u >= 0}

    }
    //    printlnGreen(s"cg for folding retiming")
    //    println(cg)
    val solutions = Try(cg.getSolution)
    solutions match {
      case Failure(exception) =>
        printlnRed(s"no solution for given folding set:\n${foldingSets.map(_.mkString(" ")).mkString("\n")}")
        Seq[Int]()
      case Success(value) => value.map(_.toInt)
    }
  }

  def isFeasible: Boolean = {
    dfg.foreachInnerEdge { edge =>
      val U = dfg.getEdgeSource(edge)
      val V = dfg.getEdgeTarget(edge)
      if (foldingFactor * dfg.getEdgeWeight(edge).toInt - U.delay + foldingOrders(V) - foldingOrders(U) < 0) return false
    }
    true
  }

  def retimed = dfg.clone().asInstanceOf[DFG[T]].retimed(solveRetiming())

  def folded(implicit holderProvider: BitCount => T) = {
    val retimedDFG = retimed
    val foldedDFG = DFG[T]
    (retimedDFG.inputNodes ++ retimedDFG.outputNodes).foreach(foldedDFG.addVertex(_))
    devices.foreach(foldedDFG.addVertex(_))
    printlnGreen(s"folded nodes: ${foldedDFG.vertexSeq.mkString(" ")}")
    retimedDFG.foreachEdge { edge => // U -> V
      val U = retimedDFG.getEdgeSource(edge)
      val V = retimedDFG.getEdgeTarget(edge)
      val source = if (U.isIO) U else deviceOf(U)
      val target = if (V.isIO) V else deviceOf(V)
      val u = if (U.isIO) foldingOrders(retimedDFG.targetsOf(U).head) else foldingOrders(U)
      val v = if (V.isIO) foldingOrders(retimedDFG.sourcesOf(V).head) else foldingOrders(V)
      val foldedDelay = if (U.isIO || V.isIO) 0 else foldingFactor * retimedDFG.getEdgeWeight(edge).toInt - U.delay + v - u
      assert(foldedDelay >= 0, s"folding constraints not met, delay of $U -> $V is ${retimedDFG.getEdgeWeight(edge)}, folded to  $foldedDelay")
      val order = edge.order

      val foldedEdge = DefaultDelay[T](Seq(Schedule(if(V.isIO) (v + U.delay) % foldingFactor else v, foldingFactor)), order)
      foldedDFG.addEdge(source, target, foldedEdge)
      foldedDFG.setEdgeWeight(foldedEdge, foldedDelay)

      printlnGreen(s"${retimedDFG.getEdgeSource(edge)} -> ${retimedDFG.getEdgeTarget(edge)} folded to $source -> $target at ${foldedEdge.schedules.mkString(" ")}, delay = $foldedDelay cycles")
    }
    foldedDFG
  }
}
