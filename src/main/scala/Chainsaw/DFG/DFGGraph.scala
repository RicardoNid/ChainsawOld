package Chainsaw.DFG

import Chainsaw._
import org.jgrapht._
import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import spinal.core._

import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable


/** Graph properties:
 * directed: yes
 * weighted: yes
 * self-loop: yes
 * parallel edge: yes
 */
class DFGGraph[T <: Data](implicit val holderProvider: BitCount => T) extends DirectedWeightedPseudograph[DSPNode[T], DSPEdge[T]](classOf[DSPEdge[T]]) {

  implicit def currentDFG: DFGGraph[T] = this

  @deprecated override def vertexSet(): util.Set[DSPNode[T]] = super.vertexSet()

  @deprecated override def edgeSet(): util.Set[DSPEdge[T]] = super.edgeSet()

  def vertexSeq: Seq[DSPNode[T]] = super.vertexSet().toSeq

  def edgeSeq: Seq[DSPEdge[T]] = super.edgeSet().toSeq

  def inputNodes: Seq[DSPNode[T]] = vertexSeq.filter(_.isInstanceOf[InputNode[T]])

  def outputNodes: Seq[DSPNode[T]] = vertexSeq.filter(_.isInstanceOf[OutputNode[T]])

  def foreachVertex(body: DSPNode[T] => Unit): Unit = vertexSeq.foreach(body)

  def foreachInnerVertex(body: DSPNode[T] => Unit): Unit = vertexSeq.filterNot(_.isIO).foreach(body)

  def foreachEdge(body: DSPEdge[T] => Unit): Unit = edgeSeq.foreach(body)

  def foreachInnerEdge(body: DSPEdge[T] => Unit): Unit = edgeSeq.filterNot(edge => edge.source.isIO || edge.target.isIO).foreach(body)

  def sourcesOf(node: DSPNode[T]): mutable.Set[DSPNode[T]] = incomingEdgesOf(node).map(_.source)

  def targetsOf(node: DSPNode[T]): mutable.Set[DSPNode[T]] = outgoingEdgesOf(node).map(_.target)

  def executionTimes: Seq[Double] = vertexSeq.map(_.exeTime)

  /** Confirm that the graph has no mux(parallel edges)
   */
  // classification
  def hasNoMux: Boolean = vertexSeq.forall(vertex => vertex.targets.distinct.size == vertex.targets.size)

  def hasNoParallelEdge: Boolean = Seq.tabulate(vertexSeq.size, vertexSeq.size)((i, j) => // for all pairs of two distinct vertices
    if (i != j) getAllEdges(vertexSeq(i), vertexSeq(j)).size() <= 1 else true) // there should be only 1 / no edge in between
    .flatten.forall(_ == true)

  def isMISO: Boolean = vertexSeq.forall(_.hardware.outWidths.size == 1)

  def isBasic = {
    val ret = hasNoMux && hasNoParallelEdge && isMISO
    if(! ret) {
      if(!hasNoMux) printlnGreen("this DFG has muxes")
      if(!hasNoParallelEdge) printlnGreen("this DFG has parallel edges")
      if(!isMISO) printlnGreen("this DFG is MIMO")
    }
    ret
  }


  // constructing graph
  override def setEdgeWeight(e: DSPEdge[T], weight: Double): Unit = {
    //    if (!this.isInstanceOf[ConstraintGraph[T]]) require(weight >= 0, s"negative delay on ${e.symbol} is not allowed")
    super.setEdgeWeight(e, weight)
  }

  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Double): Unit = {
    val inOrder = target.incomingEdges.size // default strategy
    val outOrder = 0 // default strategy
    val edge = DefaultDelay[T](outOrder, inOrder)
    if (addEdge(source, target, edge)) setEdgeWeight(edge, delay)
  }

  def addExp(exp: DSPAssignment[T]): Unit = {
    import exp._
    sources.foreach(addVertex)
    addVertex(target)
    sources.zip(delays).foreach { case (src, delay) => addEdge(src, target, delay) }
  }

  def addPath(path: DSPPath[T]): Unit = {
    import path._
    require(nodes.size == delays.size + 1)
    nodes.foreach(addVertex)
    nodes.init.zip(nodes.tail).zip(delays).foreach { case ((src, des), delay) => addEdge(src, des, delay) }
  }

  def setInput(target: DSPNode[T], inOrder: Int): InputNode[T] = {
    val inputName = s"input${inputNodes.size}"
    val inputNode = InputNode[T](inputName)
    val edge = DefaultDelay[T](0, inOrder)
    addVertex(inputNode)
    addEdge(inputNode, target, edge)
    setEdgeWeight(edge, 0)
    inputNode
  }

  def setInput(target: DSPNode[T], name: String = ""): InputNode[T] = {
    val inputName = if (name.nonEmpty) name else s"input${inputNodes.size}"
    val inputNode = InputNode[T](inputName)
    addExp(inputNode >=> 0 >=> target)
    inputNode
  }

  def setOutput(source: DSPNode[T], outOrder: Int): OutputNode[T] = {
    val outputName = s"output${outputNodes.size}"
    val outputNode = OutputNode[T](outputName)
    val edge = DefaultDelay[T](outOrder, 0)
    addVertex(outputNode)
    addEdge(source, outputNode, edge)
    setEdgeWeight(edge, 0)
    outputNode
  }

  def setOutput(source: DSPNode[T], name: String = ""): OutputNode[T] = {
    val outputName = if (name.nonEmpty) name else s"output${outputNodes.size}"
    val outputNode = OutputNode[T](outputName)
    addExp(source >=> 0 >=> outputNode)
    outputNode
  }

  // properties
  def isRecursive: Boolean = new CycleDetector(this).detectCycles()

  def isForwarding: Boolean = !isRecursive

  def delayAmount: Double = vertexSeq.map(node => (node.outgoingEdges.map(_.weight) :+ 0.0).max).sum

  /** The least common multiple of all periods of muxes in this DFG
   */
  def globalLcm: Int = edgeSeq.flatMap(_.schedules).map(_.period).reduce(lcm)

  def latency: Int = { // FIXME: find a clear definition for both SISO, MIMO, and DFG with MUX
    val algo = new alg.shortestpath.BellmanFordShortestPath(this)
    Seq.tabulate(inputNodes.size, outputNodes.size)((i, j) => algo.getPathWeight(inputNodes(i), outputNodes(j))).flatten.min.toInt
  }

  def impl: Seq[T] => Seq[T] = new DFGImpl(this).impl

  // feasibilityConstraintGraph
  def fcg: ConstraintGraph[T] = {
    val cg = ConstraintGraph[T]
    foreachInnerVertex(cg.addVertex(_))
    foreachInnerEdge(edge => cg.addConstraint(edge.source - edge.target <= edge.weight.toInt))
    cg
  }

  def retimed(solutions: Seq[Int]): DFGGraph[T] = {
    val r: Map[DSPNode[T], Int] = vertexSeq.zip(solutions).map { case (node, i) => node -> i }.toMap
    foreachInnerEdge(edge => setEdgeWeight(edge, edge.weight + r(edge.target) - r(edge.source)))
    this
  }

  override def toString: String =
    s"-----graph-----\n" +
      s"nodes:\n${vertexSeq.mkString(" ")}\n" +
      s"edges:\n${edgeSeq.map(edge => s"${edge.symbol} $edge").mkString("\n")}\n" +
      s"loops:\n${new alg.cycle.CycleDetector(this).findCycles().mkString(" ")}\n" +
      s"------end------\n"

  /** Besides nodes and edges, we clone the weights
   */
  override def clone(): AnyRef = {
    val graph = super.clone().asInstanceOf[DFGGraph[T]]
    graph.foreachEdge(edge => graph.setEdgeWeight(edge, edge.weight))
    graph
  }
}

object DFGGraph {
  def apply[T <: Data](implicit holderProvider: BitCount => T): DFGGraph[T] = new DFGGraph()
}