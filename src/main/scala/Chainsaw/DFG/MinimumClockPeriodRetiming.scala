package Chainsaw.DFG

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

import scala.util.{Try, Failure, Success}

import scala.collection.JavaConversions._
import scala.math.ceil

class MinimumClockPeriodRetiming(dfg: DFG) {

  // defined as val as they won't change through the algo
  val n = dfg.vertexSet().size()
  val tmax = dfg.executionTimes.max
  val M = tmax * n
  val vertices = dfg.vertexSet().toSeq

  def graphForComputation = {
    val ret = dfg.clone().asInstanceOf[DFG]
    ret.foreachEdge { edge =>
      val weightUpdated = dfg.getEdgeWeight(edge) * M - dfg.getEdgeSource(edge).executionTime
      ret.setEdgeWeight(edge, weightUpdated)
    }
    ret
  }

  def matrixS = {
    Array.tabulate(n, n)((u, v) => graphForComputation.shortestPath(vertices(u), vertices(v)).getWeight)
  }

  /** W(U,V) is the minimum number of registers on any path U -> V
   *
   */
  def matrixW = {
    Array.tabulate(n, n) { (u, v) =>
      if (u != v) ceil(matrixS(u)(v) / M).toInt
      else 0
    }
  }

  /** D(U,V) is the maximum computation time on any path U -> V
   *
   */
  def matrixD = {
    Array.tabulate(n, n) { (u, v) =>
      if (u != v) ceil(matrixW(u)(v) * M - matrixS(u)(v) + vertices(v).executionTime)
      else vertices(u).executionTime
    }
  }

  def determine(c: Double) = {
    val cg = dfg.basicRetimingGraph

    val r = cg.vertexSet().toSeq.tail // drop the reference node
    if (dfg.maxExecutionTime > c) Seq[Double]()
    else {
      Array.tabulate(n, n)((u, v) => if (u != v && matrixD(u)(v) > c) cg.add(r(u) - r(v) <= matrixW(u)(v) - 1))

      val solutions = Try(cg.solutions)
      solutions match {
        case Failure(exception) => Seq[Double]()
        case Success(value) => value
      }
    }
  }

  def solution = {
    val candidates = matrixD.flatten.distinct.sorted // small -> big

    // binary search
    //    def recursion(candidates: Seq[Double]): Seq[Double] = {
    //      println(s"candidates ${candidates.mkString(" ")}")
    //      if (candidates.size == 1) candidates
    //      else {
    //        val mid = (candidates.size - 1) / 2 // make sure that mid won't be the latter one
    //        val midValue = candidates(mid)
    //        val next = if (determine(midValue).nonEmpty) candidates.take(mid + 1)
    //        else candidates.takeRight(candidates.size - mid - 1)
    //        recursion(next)
    //      }
    //    }
    //
    //    (recursion(candidates).head, determine(recursion(candidates).head))
    val index = candidates.filter(determine(_).nonEmpty).min
    (index, determine(index))
  }

}
