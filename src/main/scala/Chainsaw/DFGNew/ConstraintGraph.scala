package Chainsaw.DFGNew

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


class ConstraintGraph[T <: Data](implicit holderProvider: BitCount => T) extends DFG[T] {

  // init
  val referenceNode = VoidNode[T]()
  super.addVertex(referenceNode)

  override def addVertex(constraintNode: DSPNode[T]): Boolean = {
    val succeed = super.addVertex(constraintNode)
    addEdge(referenceNode, constraintNode, 0)
    succeed
  }

  def addConstraint(constraint: DSPConstraint[T]): Unit = {
    import constraint._
    addVertex(target)
    addVertex(source)
    val e = getEdge(source, target)
    if (e == null) {addEdge(source, target, value)} else setEdgeWeight(e, value min e.weight)
  }

  def getSolution = {
    val algo = new BellmanFordShortestPath(this)
    val paths = algo.getPaths(referenceNode)
    vertexSet().toSeq.filterNot(_ == referenceNode).map(paths.getWeight(_))
  }

  override def toString: String =
    s"nodes:\n${vertexSet().mkString(" ")}\n" +
      s"edges:\n${edgeSet().map(edge => s"${edge.source}->${edge.target}, delay = ${getEdgeWeight(edge)}").mkString("\n")}\n" +
      s"cycles:\n${new alg.cycle.CycleDetector(this).findCycles().mkString(" ")}"
}

object ConstraintGraph {
  def apply[T <: Data](implicit holderProvider: BitCount => T): ConstraintGraph[T] = new ConstraintGraph[T]()
}

object testCG {
  def main(args: Array[String]): Unit = {
    val Seq(r1, r2, r3, r4) = (0 until 4).map(i => VoidNode[SInt](s"const$i"))
    val cg = ConstraintGraph[SInt]
    cg.addConstraint(r1 - r2 <= 0)
    cg.addConstraint(r3 - r1 <= 5)
    cg.addConstraint(r4 - r1 <= 4)
    cg.addConstraint(r4 - r3 <= -1)
    cg.addConstraint(r3 - r2 <= 2)
    println(cg.getSolution.mkString(" "))
    println(cg)
  }
}
