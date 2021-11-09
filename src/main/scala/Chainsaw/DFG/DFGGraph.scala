package Chainsaw.DFG

import Chainsaw._
import org.jgrapht._
import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._

import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.language.postfixOps

class DFGGraph[T <: Data](val name: String) extends DirectedWeightedPseudograph[DSPNode[T], DSPEdge[T]](classOf[DSPEdge[T]]) {

  implicit def currentDFG: DFGGraph[T] = this

  val logger: Logger = LoggerFactory.getLogger("editing DFG")

  // we deprecate vertex/edgeSet and use Seq instead, as sets may lose their orders through collection methods
  @deprecated override def vertexSet(): util.Set[DSPNode[T]] = super.vertexSet()

  @deprecated override def edgeSet(): util.Set[DSPEdge[T]] = super.edgeSet()

  def vertexSeq: Seq[DSPNode[T]] = super.vertexSet().toSeq

  def edgeSeq: Seq[DSPEdge[T]] = super.edgeSet().toSeq

  // special nodes and their traversing methods
  def inputNodes: Seq[DSPNode[T]] = vertexSeq.filter(_.isInput)

  def outputNodes: Seq[DSPNode[T]] = vertexSeq.filter(_.isOutput)

  def constantNodes: Seq[DSPNode[T]] = vertexSeq.filter(_.isConstant)

  def innerNodes: Seq[DSPNode[T]] = vertexSeq.filter(_.isInner)

  def foreachVertex(body: DSPNode[T] => Unit): Unit = vertexSeq.foreach(body)

  def foreachInnerVertex(body: DSPNode[T] => Unit): Unit = innerNodes.foreach(body)

  def foreachEdge(body: DSPEdge[T] => Unit): Unit = edgeSeq.foreach(body)

  def foreachInnerEdge(body: DSPEdge[T] => Unit): Unit = edgeSeq.filter(edge => edge.source.isInner && edge.target.isInner).foreach(body)

  def sourcesOf(node: DSPNode[T]): mutable.Set[DSPNode[T]] = incomingEdgesOf(node).map(_.source)

  def targetsOf(node: DSPNode[T]): mutable.Set[DSPNode[T]] = outgoingEdgesOf(node).map(_.target)

  def executionTimes: Seq[Double] = vertexSeq.map(_.exeTime)

  /** Confirm that the graph has no mux(parallel edges)
   */
  // classification
  def hasNoParallelEdge: Boolean = vertexSeq.forall(vertex => vertex.targets.distinct.size == vertex.targets.size)

  def hasNoMux: Boolean = edgeSeq.forall(edge => edge.schedules.size == 1 && edge.schedules.head == Schedule(0, 1))

  override def setEdgeWeight(e: DSPEdge[T], weight: Double): Unit = super.setEdgeWeight(e, weight)

  def setEdgeSchedules(e: DSPEdge[T], schedules: Seq[Schedule]): Unit = {
    addEdge(e.source(e.outOrder), e.target(e.inOrder), e.weight, schedules)
    removeEdge(e)
  }

  def addVertices(vertices: DSPNode[T]*) = vertices.foreach(addVertex)

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
  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Double, schedules: Seq[Schedule]): Unit = {
    if (!this.isInstanceOf[ConstraintGraph[T]]) {
      if (source.hardware.outWidths.size > 1) logger.warn(s"adding edge to MIMO node $source with no specified port number")
      if (target.hardware.inDegree > 1) logger.warn(s"adding to MIMO node $target with no specified port number")
    }
    addEdge(source, target, 0, target.incomingEdges.size, delay, schedules)
  }

  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Double): Unit =
    addEdge(source, target, delay, NoMUX())

  // Add edge by DSPNodeWithOrder format
  def addEdge(source: DSPNodeWithOrder[T], target: DSPNodeWithOrder[T], delay: Double, schedules: Seq[Schedule]): Unit =
    addEdge(source.node, target.node, source.order, target.order, delay, schedules)

  def addEdge(source: DSPNodeWithOrder[T], target: DSPNodeWithOrder[T], delay: Double): Unit =
    addEdge(source.node, target.node, source.order, target.order, delay)

  /** Add an expression to a basic DFG, that is, "derive" a new node by several driving nodes
   *
   * @example dfg.add(Seq(a,b) >=> (0,1) >=> c), when c is an adder, this means c.output = a.output + b.output
   */
  def addExp(exp: DSPAssignment[T]): Unit = {
    import exp._
    if (sources.exists(_.hardware.outWidths.size > 1)) logger.warn(s"using addExp(which provides no port number or MUX info) on a MIMO DFG")
    sources.foreach(addVertex)
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
    if (nodes.exists(_.hardware.outWidths.size > 1)) logger.warn(s"using addPath(which provides no port number or MUX info) on a MIMO DFG")
    nodes.foreach(addVertex)
    nodes.init.zip(nodes.tail).zip(delays).foreach { case ((src, des), delay) => addEdge(src, des, delay) }
  }

  def addInput(name: String = ""): InputNode[T] = {
    val inputName = if (name.nonEmpty) name else s"input${inputNodes.size}"
    val inputNode = InputNode[T](inputName)
    addVertex(inputNode)
    inputNode
  }

  def setInput(target: DSPNode[T], inOrder: Int = 0, name: String = "", schedules: Seq[Schedule] = NoMUX()): InputNode[T] = {
    val inputNode = addInput(name)
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

  def delayAmount: Int = vertexSeq.map { node =>
    val edgesOnPorts: Seq[Seq[DSPEdge[T]]] = node.outgoingEdges.groupBy(_.outOrder).toSeq.map(_._2)
    edgesOnPorts.map(_.map(_.delay).max).sum
  }.sum

  def unmergedDelayAmount: Int = edgeSeq.map(_.delay).sum

  def isMerged: Boolean = delayAmount == unmergedDelayAmount

  /** The least common multiple of all muxes in this DFG, which is the working period of this DFG
   */
  def globalLcm: Int = {
    val periods = edgeSeq.flatMap(_.schedules).map(_.period).distinct.sorted.reverse
    logger.debug(s"periods of MUX in $name:\n${periods.mkString(" ")}")
    periods.reduce(lcm)
  }

  def delayPaths: Seq[GraphPath[DSPNode[T], DSPEdge[T]]] = { // FIXME: need optimization! this is a big bottleneck!
    val algo = new alg.shortestpath.BellmanFordShortestPath(this)
    Seq.tabulate(inputNodes.size, outputNodes.size)((i, j) => algo.getPath(inputNodes(i), outputNodes(j))).flatten
  }

  def isHomogeneous: Boolean = delayPaths.forall(_.getWeight == delayPaths.head.getWeight)

  /** Currently, it is defined as the shortest path between inputs and outputs
   */
  def latencyPath: GraphPath[DSPNode[T], DSPEdge[T]] = delayPaths.minBy(_.getWeight) // TODO: a serious definition of latency

  def latency: Int = latencyPath.getWeight.toInt

  def criticalPathLength: Double = new CriticalPathAlgo(this).criticalPathLength

  def impl(dataIns: Seq[T], dataReset: Boolean = true)(implicit holderProvider: HolderProvider[T]): Seq[T] =
    new DFGImpl(this, dataReset)(holderProvider).impl(dataIns)

  // feasibilityConstraintGraph
  def fcg: ConstraintGraph[T] = {
    val cg = ConstraintGraph[T]()
    foreachInnerVertex(cg.addVertex(_))
    foreachInnerEdge(edge => cg.addConstraint(edge.source - edge.target <= edge.weight.toInt))
    cg
  }

  def retimed(solutions: Map[DSPNode[T], Int]): DFGGraph[T] = new Retiming(this, solutions).retimed

  def merged: DFGGraph[T] = new DFGRegOpt(this).getRegMergedDFG

  def folded(foldingSet: Seq[Seq[DSPNode[T] with Foldable[T]]]): DFGGraph[T] = new Folding(this, foldingSet).folded

  override def toString: String = {

    val inputEdges = edgeSeq.filter(edge => edge.source.isInput || edge.source.isInstanceOf[ConstantNode[T]])
    val outputEdges = edgeSeq.filter(edge => edge.target.isOutput)
    val otherEdges = edgeSeq.diff(inputEdges).diff(outputEdges)


    s"-----graph:$name-----\n" +
      s"inputs:\n\t${inputNodes.mkString(" ")}\n" +
      s"outputs:\n\t${outputNodes.mkString(" ")}\n" +
      s"constants:\n\t${constantNodes.mkString(" ")}\n" +
      s"inner nodes:\n\t${innerNodes.mkString(" ")}\n" +
      s"edges:\n" +
      s"input edges:\n\t${inputEdges.map(edge => s"${edge.symbol} $edge").mkString("\n\t")}\n" +
      s"inner edges:\n\t${otherEdges.map(edge => s"${edge.symbol} $edge").mkString("\n\t")}\n" +
      s"output edges:\n\t${outputEdges.map(edge => s"${edge.symbol} $edge").mkString("\n\t")}\n" +
      s"loops:\n\t${new alg.cycle.CycleDetector(this).findCycles().mkString(" ")}\n" +
      s"------end------\n"
  }

  /** Besides nodes and edges, we clone the weights
   */
  override def clone(): AnyRef = {
    val graph = super.clone().asInstanceOf[DFGGraph[T]]
    graph.foreachEdge(edge => graph.setEdgeWeight(edge, edge.weight))
    graph
  }

  // TODO: consider carefully on these properties
  def asNode(name: String, graphLatency: CyclesCount = latency cycles, dataReset: Boolean = false)
            (implicit holderProvider: HolderProvider[T]): DeviceNode[T] = {
    require(isForwarding)
    DeviceNode[T](
      DSPHardware(
        impl = (dataIns: Seq[T], _: GlobalCount) => impl(dataIns, dataReset), // FIXME: this won't provide the counter of outer graph, is that legal?
        inDegree = inputNodes.size,
        outWidths = Seq.fill(outputNodes.size)(-1 bits) // FIXME: what if we use subgraph in
      ),
      name,
      graphLatency,
      0.0 ns // FIXME: this could be difficult to define
    )
  }
}

object DFGGraph {
  def apply[T <: Data](name: String): DFGGraph[T] = new DFGGraph(name)
}