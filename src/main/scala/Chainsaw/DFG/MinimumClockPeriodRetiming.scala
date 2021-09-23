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

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.collection.JavaConversions._
import scala.math.ceil

class MinimumClockPeriodRetiming(dfg: DFG) {

  // defined as val as they won't change through the algo
  val n = dfg.vertexSet().size()
  val tmax = dfg.executionTimes.max
  val M = tmax * n
  val vertices = dfg.vertexSet().toSeq

  def getSolution = {

    val n = dfg.vertexSet().size()
    val tmax = dfg.executionTimes.max
    val M = tmax * n
    val vertices = dfg.vertexSet().toSeq

    val GPrime = dfg.clone().asInstanceOf[DFG]
    GPrime.foreachEdge { edge =>
      val weightUpdated = dfg.getEdgeWeight(edge) * M - dfg.getEdgeSource(edge).executionTime
      GPrime.setEdgeWeight(edge, weightUpdated)
    }
    val matrixS = Array.tabulate(n, n)((u, v) => GPrime.shortestPath(vertices(u), vertices(v)).getWeight)
    val matrixW = Array.tabulate(n, n)((u, v) => if (u != v) ceil(matrixS(u)(v) / M).toInt else 0)
    val matrixD = Array.tabulate(n, n)((u, v) =>
      if (u != v) ceil(matrixW(u)(v) * M - matrixS(u)(v) + vertices(v).executionTime) else vertices(u).executionTime)

    println("matrixS")
    println(matrixS.map(_.mkString(" ")).mkString("\n"))
    println("matrixW")
    println(matrixW.map(_.mkString(" ")).mkString("\n"))
    println("matrixD")
    println(matrixD.map(_.mkString(" ")).mkString("\n"))

    def solveC(c: Double) = {
      val cg = dfg.feasibilityConstraintGraph // basic constraint
      val r = cg.vertexSet().toSeq.tail // drop the reference node
      Array.tabulate(n, n)((u, v) => if (u != v && matrixD(u)(v) > c) cg.add(r(u) - r(v) <= matrixW(u)(v) - 1))
      val solutions = Try(cg.getSolution)
      solutions match {
        case Failure(exception) => Seq[Double]()
        case Success(value) => value
      }
    }

    val candidates = matrixD.flatten.distinct.filter(_ >= dfg.maxExecutionTime).sorted // small -> big

    // binary search
    @tailrec
    def binarySearch(candidates: Seq[Double]): Seq[Double] = {
      if (candidates.size == 1) candidates
      else {
        val mid = (candidates.size - 1) / 2 // make sure that mid won't be the latter one
        val midValue = candidates(mid)
        val next = if (solveC(midValue).nonEmpty) candidates.take(mid + 1)
        else candidates.takeRight(candidates.size - mid - 1)
        binarySearch(next)
      }
    }

    val minimunCriticalPathLength = binarySearch(candidates).head
    (minimunCriticalPathLength, solveC(minimunCriticalPathLength))
  }
}
