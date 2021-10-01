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

class Folding[T <: Data](dfg: DFG[T], foldingSets: Seq[Seq[DSPNode[T]]]) {

  val foldingFactor = foldingSets.head.length
  require(foldingSets.forall(_.size == foldingFactor))
  // node -> folding order of the node
  val foldingOrders: Map[DSPNode[T], Int] = foldingSets.map { set => set.zipWithIndex.map { case (node, i) => node -> i } }.flatten.toMap

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

  def retimed = dfg.clone().asInstanceOf[DFG[T]].retimed(solveRetiming())
}
