package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht._
import org.jgrapht.alg.shortestpath.BellmanFordShortestPath
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConversions._

case class Constraint(target: ConstraintNode, source: ConstraintNode, value: Double) {
  def <=(value: Double) = Constraint(target, source, value = value)
}

class ConstraintNode {
  def -(that: ConstraintNode) = Constraint(this, that, 0)

  override def toString: String = super.toString.takeRight(2)
}

object ConstraintNode {
  def apply(): ConstraintNode = new ConstraintNode()
}

class ConstraintGraph extends SimpleDirectedWeightedGraph[ConstraintNode, DefaultWeightedEdge](classOf[DefaultWeightedEdge]) {

  // init
  val referenceNode = ConstraintNode()
  addVertex(referenceNode)

  def add(constraint: Constraint): Unit = addConstraint(constraint.target, constraint.source, constraint.value)

  def addConstraint(targetVertex: ConstraintNode, sourceVertex: ConstraintNode, value: Double): Unit = {
    if (!vertexSet().contains(targetVertex)) {
      addVertex(targetVertex)
      val e = addEdge(referenceNode, targetVertex)
      setEdgeWeight(e, 0)
    }
    if (!vertexSet().contains(sourceVertex)) {
      addVertex(sourceVertex)
      val e = addEdge(referenceNode, sourceVertex)
      setEdgeWeight(e, 0)
    }
    if (containsEdge(sourceVertex, targetVertex)) {
      val edge = getEdge(sourceVertex, targetVertex)
      val weightUpdated = value min getEdgeWeight(edge)
      setEdgeWeight(edge, weightUpdated)
    }
    else {
      val e = addEdge(sourceVertex, targetVertex)
      setEdgeWeight(e, value)
    }
  }

  def getSolution = {
    val algo = new BellmanFordShortestPath(this)
    val paths = algo.getPaths(referenceNode)
    // TODO: more hint on the situation where no solution exists
    //    paths match {
    //      case Failure(exception) => "negative loop exists, inequality group has no solution"
    //      case Success(validPaths) =>
    //    }
    vertexSet().toSeq.filterNot(_ == referenceNode).map(paths.getWeight(_))
  }
}

object ConstraintGraph {
  def apply(): ConstraintGraph = new ConstraintGraph()
}

object testCG {
  def main(args: Array[String]): Unit = {
    val r1, r2, r3, r4 = ConstraintNode()
    val cg = ConstraintGraph()
    cg.add(r1 - r2 <= 0)
    cg.add(r3 - r1 <= 5)
    cg.add(r4 - r1 <= 4)
    cg.add(r4 - r3 <= -1)
    cg.add(r3 - r2 <= 2)
    println(cg.getSolution.mkString(" "))
  }
}
