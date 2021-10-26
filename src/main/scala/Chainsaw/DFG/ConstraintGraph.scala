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
import org.slf4j.{LoggerFactory, Logger}

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConversions._


class ConstraintGraph[T]() extends DFGGraph[T]("constraintGraph") {

  val referenceNode: GeneralNode[T] = VoidNode[T]()
  super.addVertex(referenceNode)

  def initFrom(dfg: DFGGraph[T]): Unit = dfg.vertexSeq.foreach(addVertex)

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
    if (e == null) {
      addEdge(source, target, value)
    } else setEdgeWeight(e, value min e.weight)
  }

  def getSolution: Map[DSPNode[T], Int] = {
    //    logger.info(s"solving constraint graph: $this")
    val algo = new BellmanFordShortestPath(this)
    val paths = algo.getPaths(referenceNode)
    val ret: Map[DSPNode[T], Int] = vertexSeq.filterNot(_ == referenceNode).map(node => node -> paths.getWeight(node).toInt).toMap
    val minValue = ret.values.min
    ret.map { case (node, value) => node -> (value - minValue) } // so that the minimum value is 0, but the retiming won't change
  }

  override def toString: String =
    s"nodes:\n${vertexSeq.mkString(" ")}\n" +
      s"edges:\n${edgeSeq.map(edge => s"${edge.target} - ${edge.source} <= ${edge.weight}").mkString("\n")}\n" +
      s"cycles:\n${new alg.cycle.CycleDetector(this).findCycles().mkString(" ")}"
}

object ConstraintGraph {
  def apply[T](): ConstraintGraph[T] = new ConstraintGraph[T]()

  def apply[T](dfg: DFGGraph[T]): ConstraintGraph[T] = {
    val ret = ConstraintGraph[T]()
    ret.initFrom(dfg)
    ret
  }
}


