package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DFG extends DirectedWeightedPseudograph[DSPNode, DSPEdge](classOf[DSPEdge]) {

  def inputNodes = vertexSet().filter(_.isInstanceOf[InputNode])

  def outputNodes = vertexSet().filter(_.isInstanceOf[OutputNode])

  implicit class EdgeProperties(edge: DSPEdge) {
    def target = getEdgeTarget(edge)

    def source = getEdgeSource(edge)

    def weight = getEdgeWeight(edge)
  }

  implicit class NodeProperties(node: DSPNode) {

    def outgoingEdges = outgoingEdgesOf(node)

    def incomingEdges = incomingEdgesOf(node)

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.map(_.target)

  }

  def addEdge(sourceVertex: DSPNode, targetVertex: DSPNode, delay: Int): Boolean = {
    val edge = FPGADelay() // temporarily, using FPGADelay as default
    val success = addEdge(sourceVertex, targetVertex, edge)
    setEdgeWeight(edge, delay)
    success
  }

  def addVertexFromSource(source: DSPNode, target: DSPNode, delay: Int): Unit = {
    addVertex(target)
    addEdge(source, target, delay)
  }

  def addVertexFromSources(sources: Seq[DSPNode], target: DSPNode, delays: Seq[Int]): Unit = {
    addVertex(target)
    sources.zip(delays).foreach { case (src, delay) => addVertexFromSource(src, target, delay) }
  }

  def addInput(): InputNode = {
    val inputNode = InputNode()
    addVertex(inputNode)
    inputNode
  }

  def addOutput(source: DSPNode, delay: Int): OutputNode = { // it works like "marking a signal as output"
    val outputNode = OutputNode()
    addVertexFromSource(source, outputNode, delay)
    outputNode
  }

  def addOutput(source: DSPNode): OutputNode = addOutput(source, 0)

  // properties of DFG
  def isRecursive: Boolean = {
    val algo = new alg.cycle.CycleDetector(this)
    algo.findCycles().nonEmpty
  }

  val bellman = new alg.shortestpath.BellmanFordShortestPath(this)

  def longestPathBetweenNodes(source: DSPNode, sink: DSPNode): GraphPath[DSPNode, DSPEdge] = {
    edgeSet().foreach(edge => setEdgeWeight(edge, -edge.weight))
    bellman.getPath(source, sink)
  }

  def longestDistanceBetweenNodes(source: DSPNode, sink: DSPNode) = longestPathBetweenNodes(source, sink).getWeight

  def longestPathBetweenEdges: Unit = {

  }

  val implForward = (dataIn: Seq[Bits]) => {
    case class NodeInfo(data: Bits, var done: Boolean)
    val implemented = mutable.Map[DSPNode, NodeInfo]()

    // starts from the inputs
    inputNodes.zip(dataIn).foreach { case (node, bits) => implemented += (node -> NodeInfo(node.impl(Seq(bits)), false)) }

    // look forward until all nodes are implemented
    def notDone = implemented.filterNot(_._2.done).keys // nodes that have targets not implemented

    while (implemented.size < vertexSet().size()) {
      notDone.foreach { source => // view targets of current nodes
        source.targets.filterNot(implemented.containsKey(_)).foreach { target => // view targets that have not been implemented
          if (target.sources.forall(implemented.containsKey(_))) { // if sources are ready, implement the target
            val dataIn = target.sources.map { source =>
              val edge = getEdge(source, target)
              edge.impl(implemented(source).data, edge.weight.toInt)
            }.toSeq
            val dataOut = target.impl(dataIn)
            implemented += (target -> NodeInfo(dataOut, false)) // and add the target to implemented
          }
        }
        if (source.targets.forall(implemented.containsKey(_))) implemented(source).done = true
        if (source.isInstanceOf[OutputNode]) implemented(source).done = true
      }
    }
    val dataOut = outputNodes.map(implemented(_).data)
    dataOut
  }

  val implRecursive = (dataIn: Seq[Bits]) => {
    val implemented = mutable.Map[DSPNode, Boolean]() // (node -> viewed)
    val data: Map[DSPNode, Bits] = vertexSet().map(node => if (node.implWidth == -1) node -> Bits() else node -> Bits(node.implWidth bits)).toMap // TODO: make sure that this won't change the order

    // starts from the inputs
    inputNodes.zip(dataIn).foreach { case (node, bits) =>
      data(node) := bits
      implemented += (node -> false)
    }

    // look forward until all nodes are implemented
    def tobeViewed = implemented.filter(!_._2).keys // nodes that have targets not implemented

    while (implemented.size < vertexSet().size()) {
      tobeViewed.foreach { source => // view targets of current nodes
        source.targets.filterNot(implemented.containsKey(_)).foreach { target => // view targets that have not been implemented
          val dataIn = target.sources.map { source =>
            val edge = getEdge(source, target)
            edge.impl(data(source), edge.weight.toInt)
          }.toSeq
          data(target) := target.impl(dataIn)
          implemented += (target -> target.isInstanceOf[OutputNode]) // add the target to implemented, output node need not to be viewed
        }
        implemented(source) = true // now the source has been viewed
      }
    }
    val dataOut = outputNodes.map(data(_))
    dataOut
  }
}

class Inc() extends DSPNode {

  override def impl(dataIn: Seq[Bits]): Bits = (dataIn.head.asUInt + U(1)).asBits

  override def delay: Int = 0

  override def executionTime: Double = 0.2

}

object TestDFG {

  def main(args: Array[String]): Unit = {

    // example1
    val g = new DFG()

    val dataIn = g.addInput()

    //    val inc0 = new Inc()
    val inc0 = ((dataIn: Seq[Bits]) => (dataIn(0).asUInt + U(1)).asBits)
      .asDSPNode(0, 0.2, 4, "inc")

    g.addVertexFromSource(dataIn, inc0, 0)

    val inc1 = new Inc()
    g.addVertexFromSource(inc0, inc1, 2)

    g.addOutput(inc1)

    GenRTL(new Component {
      val dataIn = in UInt (4 bits)
      val dataOut = out UInt (4 bits)
      dataOut := g.implRecursive(Seq(dataIn.asBits)).head.asUInt
    })

    // example2 Fig2.1 a
    val h = new DFG()
    val add = ((dataIn: Seq[Bits]) => (dataIn(0).asSInt + dataIn(1).asSInt).asBits)
      .asDSPNode(0, 0.2, 4, "add")
    val cmult = ((dataIn: Seq[Bits]) => (dataIn(0).asSInt * 3).resize(4).asBits)
      .asDSPNode(0, 0.8, 4, "cmult")

    val x = h.addInput()

    h.addVertexFromSource(x, add, 0)
    h.addVertexFromSource(add, cmult, 0)

    h.addEdge(cmult, add, 1)
    h.addOutput(add)

    GenRTL(new Component {
      val dataIn = in UInt (4 bits)
      val dataOut = out UInt (4 bits)
      dataOut := h.implRecursive(Seq(dataIn.asBits)).head.asUInt
    })

  }
}

/** Example from fig2.2
 *
 */
object DFGExample {
  def main(args: Array[String]): Unit = {
    val dfg = new DFG()
    val n1, n2, n3 = PrueNode(0, 1)
    val n4, n5, n6 = PrueNode(0, 2)

    dfg.addVertex(n1)
    Seq(n1, n1, n1).zip(Seq(n4, n5, n6)).zip(Seq(2, 3, 4)).foreach { case ((src, des), delay) => dfg.addVertexFromSource(src,des,delay) }
    Seq(n4, n5, n6).zip(Seq(n2, n3, n3)).zip(Seq(0,0,0)).foreach { case ((src, des), delay) => dfg.addVertexFromSource(src,des,delay) }
    Seq(n2, n3).zip(Seq(n1, n2)).zip(Seq(0,0,0)).foreach { case ((src, des), delay) => dfg.addVertexFromSource(src,des,delay) }

    println(dfg.vertexSet().mkString(" "))



  }
}

