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


/** represent a group of inequalities by a graph and solve it as a shortest path problem
 *
 * @see ''VLSI Digital Signal Processing Systems: Design and Implementation'' Chap4.3
 */
class ConstraintGraph[T <: Data]() extends DFGGraph[T]("constraintGraph") {

  val reference: VirtualNode[T] = VirtualNode[T]("reference")
  super.addVertex(reference)

  /** init a ConstraintGraph from a DFG with all its nodes
   */
  def initFrom(dfg: DFGGraph[T]): Unit = dfg.vertexSeq.foreach(addVertex)

  /** nodes in ConstraintGraph are connected to the reference node, hence we override the addVertex method
   */
  override def addVertex(constraintNode: DSPNode[T]): Boolean = {
    val succeed = super.addVertex(constraintNode)
    addEdge(reference, constraintNode, 0) // ref -> this node
    succeed
  }

  /** add an inequality to the graph
   *
   * @param constraint the in equality
   */
  def addConstraint(constraint: DSPConstraint[T]): Unit = {
    import constraint._
    addVertex(target)
    addVertex(source)
    val e = getEdge(source, target)
    if (e == null) { // dealing with parallel edges
      addEdge(source, target, value)
    } else setEdgeWeight(e, value min e.weight)
  }

  /** get the solution of the group of inequalities, it is the retiming value map of corresponding DFG
   *
   * @return a map of node -> its retiming value
   */
  def getSolution: Map[DSPNode[T], Int] = {
    logger.debug(s"solving constraint graph: $this")
    val algo = new BellmanFordShortestPath(this)
    val paths = algo.getPaths(reference) // shortest paths starts from referenceNode
    val ret: Map[DSPNode[T], Int] =
      vertexSeq.filterNot(_ == reference) // for all nodes except ref
        .map(node => node -> paths.getWeight(node).toInt).toMap // node -> shortest distance of ref -> node
    val minValue = ret.values.min
    ret.map { case (node, value) => node -> (value - minValue) } // so that the minimum value is 0, but the retiming won't change
  }

  /** show ConstraintGraph as a group of inequalities
   */
  override def toString: String =
    s"nodes:\n${vertexSeq.mkString(" ")}\n" +
      s"edges:\n${edgeSeq.map(edge => s"${edge.target} - ${edge.source} <= ${edge.weight}").mkString("\n")}\n" +
      s"cycles:\n${new alg.cycle.CycleDetector(this).findCycles().mkString(" ")}"
}

object ConstraintGraph {
  def apply[T <: Data](): ConstraintGraph[T] = new ConstraintGraph[T]()

  def apply[T <: Data](dfg: DFGGraph[T]): ConstraintGraph[T] = {
    val ret = ConstraintGraph[T]()
    ret.initFrom(dfg)
    ret
  }

  def apply[T <: Data](constraints:Seq[DSPConstraint[T]]): ConstraintGraph[T] = {
    val ret = ConstraintGraph[T]()
    constraints.foreach(ret.addConstraint(_))
    ret
  }
}


