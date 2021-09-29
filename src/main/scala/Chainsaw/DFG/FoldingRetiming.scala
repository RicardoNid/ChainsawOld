package Chainsaw.DFG

import scala.util.{Failure, Success, Try}
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.math.{ceil, floor}
import scala.collection.JavaConversions._

class FoldingRetiming(dfg: DFG, foldingSets: Seq[Seq[DSPNode]]) {

  val foldingFactor = foldingSets.head.length
  require(foldingSets.forall(_.size == foldingFactor))
  val foldingOrders: Map[DSPNode, Int] = foldingSets.map { set => set.zipWithIndex.map { case (node, i) => node -> i } }.flatten.toMap
  println(foldingOrders.mkString(" "))

  def solveRetiming() = {
    val cg = dfg.feasibilityConstraintGraph // basic constraint
    printlnGreen(cg.vertexSet().mkString(" "))
    cg.edgeSet().foreach(edge => println(s"${cg.getEdgeSource(edge)} - ${cg.getEdgeTarget(edge)} <= ${cg.getEdgeWeight(edge)}"))
    dfg.foreachEdge { edge => // for each edge, add the folding equation constraint as extra constraint
      val U = dfg.getEdgeSource(edge)
      val V = dfg.getEdgeTarget(edge)
      val u = foldingOrders(U)
      val v = foldingOrders(V)
      val w = dfg.getEdgeWeight(edge)
      printlnGreen(dfg.vertexSet().mkString(" "))
      printlnGreen(cg.vertexSet().toSeq.tail.mkString(" "))
      println(s"source: $U, target: $V, u = $u, v = $v, w(e) = $w, N = $foldingFactor")

      // Nw(e) - P_u + v - u >= 0
      println(floor((v - U.delay - u).toDouble / foldingFactor))
//      println(s"${r(U)} - ${r(V)} <= ${w + (v - U.delay - u).toDouble / foldingFactor}")
      cg.add(U - V <= w + floor((v - U.delay - u).toDouble / foldingFactor))
    }
    printlnGreen(cg.vertexSet().mkString(" "))
    cg.edgeSet().foreach(edge => println(s"${cg.getEdgeSource(edge)} - ${cg.getEdgeTarget(edge)} <= ${cg.getEdgeWeight(edge)}"))

    val solutions = Try(cg.getSolution)
    solutions match {
      case Failure(exception) => {
        println("no solution")
        Seq[Double]()
      }
      case Success(value) => value
    }
  }

}
