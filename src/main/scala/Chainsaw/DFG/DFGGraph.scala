package Chainsaw.DFG

import Chainsaw._
import org.jgrapht._
import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import spinal.core._

import java.util
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.math._

import org.slf4j.{LoggerFactory, Logger}

/** Main class of DFG model
 *
 * @param holderProvider function which generates a holder of a known/unknown width signal
 * @tparam T
 * learn more: [[]]
 */
class DFGGraph[T <: Data](implicit val holderProvider: BitCount => T) extends DirectedWeightedPseudograph[DSPNode[T], DSPEdge[T]](classOf[DSPEdge[T]]) {

  implicit def currentDFG: DFGGraph[T] = this
  val logger = LoggerFactory.getLogger(classOf[DFGGraph[T]])

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
  def hasNoParallelEdge: Boolean = vertexSeq.forall(vertex => vertex.targets.distinct.size == vertex.targets.size)

  def hasNoMux: Boolean = edgeSeq.forall(edge => edge.schedules.size == 1 && edge.schedules.head == Schedule(0, 1))

  override def setEdgeWeight(e: DSPEdge[T], weight: Double): Unit = super.setEdgeWeight(e, weight)

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
    if(source.hardware.outWidths.size > 1) logger.warn("adding edge to MIMO node with no specified port number")
    addEdge(source, target, 0, target.incomingEdges.size, delay, schedules)
  }

  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Double): Unit = {
    if(source.hardware.outWidths.size > 1) logger.warn("adding edge to MIMO node with no specified port number")
    addEdge(source, target, 0, target.incomingEdges.size, delay)
  }

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
    if (sources.exists(_.hardware.outWidths.size > 1)) printlnYellow(s"warning: using addExp(which provides no port number or MUX info) on a MIMO DFG")
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
    // TODO: using a solid ERROR/WARNING/INFO library instead of do println in different colors
    if (nodes.exists(_.hardware.outWidths.size > 1)) printlnYellow(s"warning: using addPath(which provides no port number or MUX info) on a MIMO DFG")
    nodes.foreach(addVertex)
    nodes.init.zip(nodes.tail).zip(delays).foreach { case ((src, des), delay) => addEdge(src, des, delay) }
  }

  def setInput(target: DSPNode[T], inOrder: Int = 0, name: String = "", schedules: Seq[Schedule] = NoMUX()): InputNode[T] = {
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

  def addRegularGraph(hardwareSeq : Seq[DSPNode[T] with Foldable[T]], diagdirect : String, digdelay : Double) = {

    require(hardwareSeq.map(_.hardware.inDegree) == hardwareSeq.map(_.hardware.outWidths.size))
    require(hardwareSeq.forall(_.hardware.inDegree == 3))
    require(diagdirect == "Positive" | diagdirect == "Negtive")

    val size: Int = floor(sqrt(hardwareSeq.size)).toInt
    val nodes = Seq.tabulate(size, size)((i, j) => hardwareSeq(size*i + j))


      nodes.foreach(_.foreach(addVertex(_)))
      nodes.foreach { i =>
        val indexi = nodes.indexOf(i)
        i.foreach { j =>
          val indexj = nodes(indexi).indexOf(j)
          if(indexi == 0 | indexj ==0) {
            if(diagdirect == s"Positive") {
              val output_ij = OutputNode[T](s"output_${indexi}_${indexj}")
              addVertex(output_ij)
              addEdge(nodes(indexi)(indexj) , output_ij , 2 , 0 , digdelay)
            }
            else {
              val input_ij = InputNode[T](s"input_${indexi}_${indexj}")
              addVertex(input_ij)
              addEdge(input_ij , nodes(indexi)(indexj) , 0 , 2 , digdelay )
            }
              if(indexi == 0) {
                setInput(nodes(indexi)(indexj) , 0 , s"input_down_$indexj")
                if(indexj == 0) setInput(nodes(indexi)(indexj) , 1 , s"input_right_$indexi")
                else addEdge(nodes(indexi)(indexj - 1) , nodes(indexi)(indexj) , 1 , 1 , 0)
              }
              else{
                addEdge(nodes(indexi - 1)(indexj) , nodes(indexi)(indexj) , 0 , 0 , 0)
                setInput(nodes(indexi)(indexj) , 1 , s"input_right_$indexi")
              }
          }
          else {
            addEdge(nodes(indexi - 1)(indexj) , nodes(indexi)(indexj) , 0 , 0 , 0)
            addEdge(nodes(indexi)(indexj - 1) , nodes(indexi)(indexj) , 1 , 1 , 0)
            if(diagdirect == s"Positive") addEdge(nodes(indexi)(indexj) , nodes(indexi - 1)(indexj - 1) , 2 , 2 , digdelay)
            else addEdge(nodes(indexi -1)(indexj - 1) , nodes(indexi)(indexj) , 2 , 2 , digdelay)
          }

          if(indexi == size - 1 | indexj == size - 1) {
            if(indexj == size - 1) {

              if(diagdirect == s"Positive") {
                val input_ij = InputNode[T](s"input_${indexi}_${indexj}")
                addVertex(input_ij)
                addEdge(input_ij , nodes(indexi)(indexj), 0 , 2 , digdelay)
              }
              else {
                val output_ij = OutputNode[T](s"output_${indexi}_${indexj}")
                addVertex(output_ij)
                addEdge(nodes(indexi)(indexj), output_ij ,  2 , 0 , digdelay)
              }
              if(indexi == size - 1) setOutput(nodes(indexi)(indexj) , 0 , s"output_down_$indexj")
              setOutput(nodes(indexi)(indexj) , 1 , s"output_right_$indexi")
            }
            else {

              if(diagdirect == s"Positive") {
                val input_ij = InputNode[T](s"input_${indexi}_${indexj}")
                addVertex(input_ij)
                addEdge(input_ij , nodes(indexi)(indexj), 0 , 2 , digdelay)
              }
              else {
                val output_ij = OutputNode[T](s"output_${indexi}_${indexj}")
                addVertex(output_ij)
                addEdge(nodes(indexi)(indexj), output_ij ,  2 , 0 , digdelay)
              }

              setOutput(nodes(indexi)(indexj) , 0 , s"output_down_$indexj")
            }
          }
        }
      }
      this
    /*else {
      nodes.foreach(_.foreach(addVertex(_)))
      nodes.foreach { i =>
        val indexi = nodes.indexOf(i)
        i.foreach { j =>
          val indexj = nodes(indexi).indexOf(j)

          if(indexi == 0 | indexj ==0) {

            if(diagdirect == s"Positive") {
              val output_ij = OutputNode[T](s"output_($indexi , $indexj)")
              addVertex(output_ij)
              addEdge(nodes(indexi)(indexj) , output_ij , 2 , 0 , digdelay)
            }
            else {
              val input_ij = InputNode[T](s"input_($indexi , $indexj)")
              addVertex(input_ij)
              addEdge(input_ij , nodes(indexi)(indexj) , 0 , 2 , digdelay )
            }

            if(indexi == 0) {
              setInput(nodes(indexi)(indexj) , 0 , s"input_down_$indexj")
            }
            else{
              addPath(nodes(indexi - 1)(indexj) >> nodes(indexi)(indexj))
            }

          }

          else {
            addPath(nodes(indexi - 1)(indexj) >> nodes(indexi)(indexj))
            if(diagdirect == s"Positive") addEdge(nodes(indexi)(indexj) , nodes(indexi - 1)(indexj - 1) , 2 , 2 , digdelay)
            else addEdge(nodes(indexi -1)(indexj - 1) , nodes(indexi)(indexj) , 2 , 2 , digdelay)
          }

          if(indexi == size - 1 | indexj == size - 1) {
            if(indexj == size - 1) {

              if(diagdirect == s"Positive") {
                val input_ij = InputNode[T](s"input_($indexi , $indexj)")
                addVertex(input_ij)
                addEdge(input_ij , nodes(indexi)(indexj), 0 , 2 , digdelay)
              }
              else {
                val output_ij = OutputNode[T](s"output_($indexi , $indexj)")
                addVertex(output_ij)
                addEdge(nodes(indexi)(indexj), output_ij ,  2 , 0 , digdelay)
              }
              if(indexi == size - 1) setOutput(nodes(indexi)(indexj) , 0 , s"output_down_$indexj")
            }
            else {

              if(diagdirect == s"Positive") {
                val input_ij = InputNode[T](s"input_($indexi , $indexj)")
                addVertex(input_ij)
                addEdge(input_ij , nodes(indexi)(indexj), 0 , 2 , digdelay)
              }
              else {
                val output_ij = OutputNode[T](s"output_($indexi , $indexj)")
                addVertex(output_ij)
                addEdge(nodes(indexi)(indexj), output_ij ,  2 , 0 , digdelay)
              }

              setOutput(nodes(indexi)(indexj) , 0 , s"output_down_$indexj")

            }
          }
        }
      }
      this
    }
    */

  }

  // properties
  def isRecursive: Boolean = new CycleDetector(this).detectCycles()

  def isForwarding: Boolean = !isRecursive

  def delayAmount: Double = vertexSeq.map(node => (node.outgoingEdges.map(_.weight) :+ 0.0).max).sum

  /** The least common multiple of all muxes in this DFG
   */
  def globalLcm: Int = edgeSeq.flatMap(_.schedules).map(_.period).sorted.reverse.reduce(lcm)

  def latencyPath: GraphPath[DSPNode[T], DSPEdge[T]] = {
    val algo = new alg.shortestpath.BellmanFordShortestPath(this)
    Seq.tabulate(inputNodes.size, outputNodes.size)((i, j) => algo.getPath(inputNodes(i), outputNodes(j))).flatten.minBy(_.getWeight)
  }

  // FIXME: find a clear definition for both SISO, MIMO, and DFG with MUX
  def latency: Int = latencyPath.getWeight.toInt

  def impl: Seq[T] => Seq[T] = new DFGImpl(this).impl

  // feasibilityConstraintGraph
  def fcg: ConstraintGraph[T] = {
    val cg = ConstraintGraph[T]
    foreachInnerVertex(cg.addVertex(_))
    foreachInnerEdge(edge => cg.addConstraint(edge.source - edge.target <= edge.weight.toInt))
    cg
  }

  def retimed(solutions: Seq[Int]): DFGGraph[T] = {
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