package Chainsaw.DFG

import Chainsaw._
import org.jgrapht._
import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import spinal.core._

import java.util
import scala.collection.JavaConversions._


/**
 * @param holderProvider
 * @tparam T
 * learn more: [[]]
 */
class DFGGraph[T <: Data](implicit val holderProvider: BitCount => T) extends DirectedWeightedPseudograph[DSPNode[T], DSPEdge[T]](classOf[DSPEdge[T]]) {

  implicit def currentDFG = this

  @deprecated override def vertexSet(): util.Set[DSPNode[T]] = super.vertexSet()

  @deprecated override def edgeSet(): util.Set[DSPEdge[T]] = super.edgeSet()

  def vertexSeq = super.vertexSet().toSeq

  def edgeSeq = super.edgeSet().toSeq

  def inputNodes = vertexSeq.filter(_.isInstanceOf[InputNode[T]])

  def outputNodes = vertexSeq.filter(_.isInstanceOf[OutputNode[T]])

  def foreachVertex(body: DSPNode[T] => Unit) = vertexSeq.foreach(body)

  def foreachInnerVertex(body: DSPNode[T] => Unit) = vertexSeq.filterNot(_.isIO).foreach(body)

  def foreachEdge(body: DSPEdge[T] => Unit) = edgeSeq.foreach(body)

  def foreachInnerEdge(body: DSPEdge[T] => Unit) = edgeSeq.filterNot(edge => edge.source.isIO || edge.target.isIO).foreach(body)

  def sourcesOf(node: DSPNode[T]) = incomingEdgesOf(node).map(_.source)

  def targetsOf(node: DSPNode[T]) = outgoingEdgesOf(node).map(_.target)

  def executionTimes = vertexSeq.map(_.exeTime)

  /** Confirm that the graph has no mux(parallel edges)
   */
  // classification
  def hasNoParallelEdge = vertexSeq.forall(vertex => vertex.targets.distinct.size == vertex.targets.size)

  def hasNoMux = edgeSeq.forall(edge => edge.schedules.size == 1 && edge.schedules.head == Schedule(0, 1))

  // constructing graph
  override def setEdgeWeight(e: DSPEdge[T], weight: Double): Unit = {
    //    if (!this.isInstanceOf[ConstraintGraph[T]]) require(weight >= 0, s"negative delay on ${e.symbol} is not allowed")
    super.setEdgeWeight(e, weight)
  }

  @deprecated // mark the original addEdge as deprecated to ask the developer/user to use methods we provide
  override def addEdge(sourceVertex: DSPNode[T], targetVertex: DSPNode[T], e: DSPEdge[T]): Boolean = super.addEdge(sourceVertex, targetVertex, e)

  /** Add edge with full information, it is the basic method for adding edge in DFG
   *
   * @param source    source node
   * @param target    target node
   * @param outOrder  the output port number of source node
   * @param inOrder   the input port number of target node
   * @param delay     delay(weight) of this edge
   * @param schedules schedules on this edge
   */
  def addEdge(source: DSPNode[T], target: DSPNode[T], outOrder: Int, inOrder: Int, delay: Double, schedules: Seq[Schedule] = NoMUX()): Unit = {
    val edge = DefaultDelay[T](schedules, outOrder, inOrder)
    if (super.addEdge(source, target, edge)) setEdgeWeight(edge, delay)
  }

  // Add edge into basicDFG(MISO, no MUX)
  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Double, schedules: Seq[Schedule]): Unit =
    addEdge(source, target, 0, target.incomingEdges.size, delay, schedules)

  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Double): Unit =
    addEdge(source, target, 0, target.incomingEdges.size, delay)

  // Add edge by DSPNodeWithOrder format
  def addEdge(source: DSPNodeWithOrder[T], target: DSPNodeWithOrder[T], delay: Double, schedules: Seq[Schedule]): Unit =
    addEdge(source.node, target.node, source.order, target.order, delay, schedules)

  def addEdge(source: DSPNodeWithOrder[T], target: DSPNodeWithOrder[T], delay: Double): Unit =
    addEdge(source.node, target.node, source.order, target.order, delay)

  /** Add an expression to a basic DFG, that is, "derive" a new node by several driving nodes
   * @example dfg.add(Seq(a,b) >=> (0,1) >=> c), when c is an adder, this means c.output = a.output + b.output
   */
  def addExp(exp: DSPAssignment[T]): Unit = {
    import exp._
    if (sources.exists(_.hardware.outWidths.size > 1)) printlnYellow(s"warning: using addExp(which provides no port number or MUX info) on a MIMO DFG")
    sources.foreach(addVertex(_))
    addVertex(target)
    sources.zip(delays).foreach { case (src, delay) => addEdge(src, target, delay) }
  }

  /** Add a interleaved sequence of nodes and delays to a basic DFG
   *
   * @example dfg.addPath(a >> b >> 1 >> c), there's no delay between a and b, 1 delay between b and c
   */
  def addPath(path: DSPPath[T]): Unit = {
    import path._
    require(nodes.size == delays.size + 1)
    // TODO: using a solid ERROR/WARNING/INFO library instead of do println in different colors
    if (nodes.exists(_.hardware.outWidths.size > 1)) printlnYellow(s"warning: using addPath(which provides no port number or MUX info) on a MIMO DFG")
    nodes.foreach(addVertex)
    nodes.init.zip(nodes.tail).zip(delays).foreach { case ((src, des), delay) => addEdge(src, des, delay) }
  }

  def setInput(target: DSPNode[T], inOrder: Int = 0, name: String = "", schedules: Seq[Schedule] = NoMUX()) = {
    val inputName = if (name.nonEmpty) name else s"input${inputNodes.size}"
    val inputNode = InputNode[T](inputName)
    addVertex(inputNode)
    addEdge(inputNode(0), target(inOrder), 0, schedules)
    inputNode
  }

  def setOutput(source: DSPNode[T], outOrder: Int = 0, name: String = "", schedules: Seq[Schedule] = NoMUX()): OutputNode[T] = {
    val outputName = if (name.nonEmpty) name else s"output${outputNodes.size}"
    val outputNode = OutputNode[T](outputName)
    addVertex(outputNode)
    addEdge(source(outOrder), outputNode(0), 0, schedules)
    outputNode
  }

  // properties
  def isRecursive: Boolean = new CycleDetector(this).detectCycles()

  def isForwarding: Boolean = !isRecursive

  def delayAmount = vertexSeq.map(node => (node.outgoingEdges.map(_.weight) :+ 0.0).max).sum

  /** The least common multiple of all muxes in this DFG
   */
  def globalLcm = edgeSeq.map(_.schedules).flatten.map(_.period).sorted.reverse.reduce(lcm(_, _))

  def latencyPath = {
    val algo = new alg.shortestpath.BellmanFordShortestPath(this)
    Seq.tabulate(inputNodes.size, outputNodes.size)((i, j) => algo.getPath(inputNodes(i), outputNodes(j))).flatten.minBy(_.getWeight)
  }

  // FIXME: find a clear definition for both SISO, MIMO, and DFG with MUX
  def latency = latencyPath.getWeight.toInt


  def impl = new DFGImpl(this).impl

  // feasibilityConstraintGraph
  def fcg = {
    val cg = ConstraintGraph[T]
    foreachInnerVertex(cg.addVertex(_))
    foreachInnerEdge(edge => cg.addConstraint(edge.source - edge.target <= edge.weight.toInt))
    cg
  }

  def retimed(solutions: Seq[Int]) = {
    val r: Map[DSPNode[T], Int] = vertexSeq.filterNot(_.isIO).zip(solutions).map { case (node, i) => node -> i }.toMap
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