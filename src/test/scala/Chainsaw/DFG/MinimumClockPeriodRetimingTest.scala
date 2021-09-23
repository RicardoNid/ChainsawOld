package Chainsaw.DFG

import org.scalatest.flatspec.AnyFlatSpec

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class MinimumClockPeriodRetimingTest extends AnyFlatSpec {

  behavior of "MinimumClockPeriodRetimingTest"

  it should "solve fig 4.2 correctly" in {

    val dfg = new DFG() // Fig 4.2 a)
    val Seq(n1, n2, n3, n4) = Seq(1, 1, 2, 2).zipWithIndex.map { case (exeTime, i) => AbstractNode(0, exeTime, s"n$i") }
    Seq(n1, n2, n3, n4).foreach(dfg.addVertex(_))
    dfg.addVertexFromSource(n2, n1, 1)
    dfg.addVertexFromSource(n1, n3, 1)
    dfg.addVertexFromSource(n1, n4, 2)
    dfg.addVertexFromSource(n3, n2, 0)
    dfg.addVertexFromSource(n4, n2, 0)

    println(dfg.feasibilityConstraintGraph.getSolution)

    val algo = new MinimumClockPeriodRetiming(dfg)
    val solution = algo.getSolution._2
    val golden = Seq(-1,0,-1,-1)
    assert(golden.zip(solution).forall{ case (i, d) => i == d.toInt})
    dfg.applyRetiming(solution.map(_.toInt))
    assert(dfg.criticalPathLength == 2)

  }

  it should "solve the problem 2 correctly" in {

    val problem2 = new DFG()
    val Seq(a, b, c, d) = Seq(20, 10, 10, 5).zipWithIndex.map { case (exeTime, i) => AbstractNode(0, exeTime, s"n$i") }
    problem2.addVertex(a)
    problem2.addVertexFromSource(a, b, 0)
    problem2.addVertexFromSource(b, c, 1)
    problem2.addVertexFromSource(c, d, 0)
    problem2.addVertexFromSource(c, a, 1)
    problem2.addVertexFromSource(d, b, 0)
    problem2.mergeDelays()

    println(problem2.feasibilityConstraintGraph.getSolution.mkString(" "))

    val cris = problem2.criticalPathGraph
    println(cris.edgeSet().toSeq.map(cris.getEdgeWeight(_)).mkString(" "))

    println(s"before: \ncritical path = ${problem2.criticalPathLength} \ndelay units in total = ${problem2.delaysCount}")
    val solution2 = new MinimumClockPeriodRetiming(problem2).getSolution._2
    problem2.applyRetiming(solution2.map(_.toInt))
    println(s"after : \ncritical path = ${problem2.criticalPathLength} \ndelay units in total = ${problem2.delaysCount}")
  }

  it should "solve the problem 2 correctly" in {

    val problem2 = new DFG()
    val Seq(a, b, c, d) = Seq(20, 10, 10, 5).zipWithIndex.map { case (exeTime, i) => AbstractNode(0, exeTime, s"n$i") }
    problem2.addVertex(a)
    problem2.addVertexFromSource(a, b, 0)
    problem2.addVertexFromSource(b, c, 1)
    problem2.addVertexFromSource(c, d, 0)
    problem2.addVertexFromSource(c, a, 1)
    problem2.addVertexFromSource(d, b, 0)
    problem2.mergeDelays()

    println(problem2.feasibilityConstraintGraph.getSolution.mkString(" "))

    val cris = problem2.criticalPathGraph
    println(cris.edgeSet().toSeq.map(cris.getEdgeWeight(_)).mkString(" "))

    println(s"before: \ncritical path = ${problem2.criticalPathLength} \ndelay units in total = ${problem2.delaysCount}")
    val solution2 = new MinimumClockPeriodRetiming(problem2).getSolution._2
    problem2.applyRetiming(solution2.map(_.toInt))
    println(s"after : \ncritical path = ${problem2.criticalPathLength} \ndelay units in total = ${problem2.delaysCount}")
  }

}
