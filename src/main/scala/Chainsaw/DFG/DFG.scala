package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht._
import org.jgrapht.alg._
import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import java.util
import scala.math.min
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import scala.util.{Try, Success, Failure}


/** Graph properties:
 * directed: yes
 * weighted: yes
 * self-loop: no
 * parallel edge: no
 */
class DFG[T <: Data](implicit holderProvider: BitCount => T) extends DirectedWeightedPseudograph[DSPNode[T], DSPEdge[T]](classOf[DSPEdge[T]]) {

  def vertexSeq = vertexSet().toSeq

  def edgeSeq = edgeSet().toSeq

  def inputNodes = vertexSeq.filter(_.isInstanceOf[InputNode[T]])

  def outputNodes = vertexSeq.filter(_.isInstanceOf[OutputNode[T]])

  implicit class EdgeProperties(edge: DSPEdge[T]) {
    def target = getEdgeTarget(edge)

    def source = getEdgeSource(edge)

    def weight = getEdgeWeight(edge).toInt

    def symbol = s"$source -> $target"
  }

  implicit class NodeProperties(node: DSPNode[T]) {

    def outgoingEdges = outgoingEdgesOf(node).toSeq

    def incomingEdges = incomingEdgesOf(node).toSeq

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.map(_.target)

  }

  def foreachVertex(body: DSPNode[T] => Unit) = vertexSeq.foreach(body)

  def foreachInnerVertex(body: DSPNode[T] => Unit) = vertexSeq.filterNot(_.isIO).foreach(body)

  def foreachEdge(body: DSPEdge[T] => Unit) = edgeSeq.foreach(body)

  def foreachInnerEdge(body: DSPEdge[T] => Unit) = edgeSeq.filterNot(edge => edge.source.isIO || edge.target.isIO).foreach(body)

  def sourcesOf(node: DSPNode[T]) = incomingEdgesOf(node).map(_.source)

  def targetsOf(node: DSPNode[T]) = outgoingEdgesOf(node).map(_.target)

  def executionTimes = vertexSeq.map(_.exeTime)

  // constructing graph
  override def setEdgeWeight(e: DSPEdge[T], weight: Double): Unit = {
    if (!this.isInstanceOf[ConstraintGraph[T]]) require(weight >= 0, s"negative delay on ${e.symbol} is not allowed")
    super.setEdgeWeight(e, weight)
  }

  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Int): Unit = {
    val inOrder = target.incomingEdges.size // default strategy
    val outOrder = 0 // default strategy
    val edge = DefaultDelay[T](Seq(Schedule(1, 1)), inOrder, outOrder)
    if (addEdge(source, target, edge)) setEdgeWeight(edge, delay)
  }

  def addExp(exp: DSPAssignment[T]): Unit = {
    import exp._
    sources.foreach(addVertex(_))
    addVertex(target)
    sources.zip(delays).foreach { case (src, delay) => addEdge(src, target, delay) }
  }

  def addPath(path: DSPPath[T]): Unit = {
    import path._
    require(nodes.size == delays.size + 1)
    nodes.foreach(addVertex(_))
    nodes.init.zip(nodes.tail).zip(delays).foreach { case ((src, des), delay) => addEdge(src, des, delay) }
  }

  def setInput(target: DSPNode[T], name: String = "") = {
    val inputName = if (name.nonEmpty) name else s"input${inputNodes.size}"
    val inputNode = InputNode[T](inputName)
    addExp(inputNode >=> 0 >=> target)
    inputNode
  }

  def setOutput(source: DSPNode[T], name: String = "") = {
    val outputName = if (name.nonEmpty) name else s"output${outputNodes.size}"
    val outputNode = OutputNode[T](outputName)
    addExp(source >=> 0 >=> outputNode)
    outputNode
  }

  // properties
  def isRecursive: Boolean = new CycleDetector(this).detectCycles()

  def delayAmount = vertexSeq.map(node => (node.outgoingEdges.map(_.weight) :+ 0).max).sum

  def globalLcm = edgeSeq.map(_.schedules).flatten.map(_.period).reduce(lcm(_, _))

  //
  val impl: Seq[T] => Seq[T] = (dataIns: Seq[T]) => {
    val signalMap: Map[DSPNode[T], Seq[T]] = vertexSeq.map(node => // a map to connect nodes with their outputs(placeholder)
      node -> node.hardware.outWidths.map(i => if (i.value == -1) holderProvider(-1 bits) else holderProvider(i))).toMap

    // create the global counter
    val globalCounter = CounterFreeRun(globalLcm)
    globalCounter.value.setName("globalCounter")

    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node).head := bits }
    vertexSeq.diff(inputNodes).foreach { target =>
      val dataIns: Seq[Seq[DSPEdge[T]]] = target.incomingEdges.groupBy(_.inOrder).toSeq.sortBy(_._1).map(_._2)
      val dataInsOnPorts = dataIns.zipWithIndex.map { case (dataInsOnePort, i) =>
        val dataCandidates: Seq[T] = dataInsOnePort.map(edge => edge.impl(signalMap(edge.source), edge.weight).apply(edge.outOrder))
        // showing mux infos
        //        println(s"mux to target $target:\n" +
        //          s"${dataInsOnePort.map(_.schedules.mkString(" ")).mkString("\n")}")
        val mux = DFGMUX[T](dataInsOnePort.map(_.schedules))
        mux.impl(dataCandidates, globalCounter.value, globalLcm)
      }
      val placeholders = signalMap(target)
      val rets = target.hardware.impl(dataInsOnPorts)
      placeholders.zip(rets).foreach { case (placeholder, ret) => placeholder := ret.resized }
      if (placeholders.size == 1) placeholders.head.setName(target.name) // name these signals
      else placeholders.zipWithIndex.foreach{ case (placeholder, i) => placeholder.setName(s"${target}_$i")}
    }
    outputNodes.map(signalMap(_)).flatten
  }

  // feasibilityConstraintGraph
  def fcg = {
    val cg = ConstraintGraph[T]
    foreachInnerVertex(cg.addVertex(_))
    foreachInnerEdge(edge => cg.addConstraint(edge.source - edge.target <= edge.weight))
    cg
  }

  def retimed(solutions: Seq[Int]) = {
    val r: Map[DSPNode[T], Int] = vertexSeq.zip(solutions).map { case (node, i) => node -> i }.toMap
    foreachInnerEdge(edge => setEdgeWeight(edge, edge.weight + r(edge.target) - r(edge.source)))
    this
  }

  override def toString: String =
    s"nodes:\n${vertexSeq.mkString(" ")}\n" +
      s"edges:\n${edgeSeq.map(edge => s"${edge.symbol} $edge, ${edge.weight} cycle").mkString("\n")}\n" +
      s"cycles:\n${new alg.cycle.CycleDetector(this).findCycles().mkString(" ")}"

  /** Besides nodes and edges, we clone the weights
   */
  override def clone(): AnyRef = {
    val graph = super.clone().asInstanceOf[DFG[T]]
    graph.foreachEdge(edge => graph.setEdgeWeight(edge, edge.weight))
    graph
  }
}

object DFG {
  def apply[T <: Data](implicit holderProvider: BitCount => T): DFG[T] = new DFG()
}