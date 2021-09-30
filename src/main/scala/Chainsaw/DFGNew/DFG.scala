package Chainsaw.DFGNew

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

import scala.math.min
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/** Graph properties:
 * directed: yes
 * weighted: yes
 * self-loop: no
 * parallel edge: no
 */
class DFG[T <: Data](implicit holderProvider: BitCount => T) extends DefaultDirectedWeightedGraph[DSPNode[T], DSPEdge[T]](classOf[DSPEdge[T]]) {

  def inputNodes = vertexSet().filter(_.isInstanceOf[InputNode[T]])

  def outputNodes = vertexSet().filter(_.isInstanceOf[OutputNode[T]])

  implicit class EdgeProperties(edge: DSPEdge[T]) {
    def target = getEdgeTarget(edge)

    def source = getEdgeSource(edge)

    def weight = getEdgeWeight(edge).toInt
  }

  implicit class NodeProperties(node: DSPNode[T]) {

    def outgoingEdges = outgoingEdgesOf(node).toSeq

    def incomingEdges = incomingEdgesOf(node).toSeq

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.map(_.target)

  }

  def foreachVertex(body: DSPNode[T] => Unit) = vertexSet().foreach(body)

  def foreachEdge(body: DSPEdge[T] => Unit) = edgeSet().foreach(body)

  // constructing graph
  override def addEdge(sourceVertex: DSPNode[T], targetVertex: DSPNode[T]): DSPEdge[T] = super.addEdge(sourceVertex, targetVertex)

  def addEdge(source: DSPNode[T], target: DSPNode[T], delay: Int): Unit = {
    val order = target.incomingEdges.size
    val edge = DefaultDelay[T](s"${source.name} -> ${target.name}", Seq(Schedule(1, 1)), order)
    println(this)
    addEdge(source, target, edge)
    setEdgeWeight(edge, delay)
  }

  def addExp(exp: DSPExpression[T]): Unit = {
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

  //
  val impl = (dataIns: Seq[T]) => {
    val signalMap: Map[DSPNode[T], T] = vertexSet().toSeq.map(node => // a map to connet nodes with their anchors
      if (node.implWidth == -1) node -> holderProvider(-1 bits)
      else node -> holderProvider(node.implWidth)).toMap

    // create the global counter
    val globalLcm = edgeSet().map(_.schedules).flatten.map(_.period).reduce(lcm(_, _))
    val globalCounter = CounterFreeRun(globalLcm)

    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node) := bits }

    vertexSet().diff(inputNodes).foreach { target =>
      if (!target.isInstanceOf[InputNode[T]]) {
        val dataIns: Seq[Seq[DSPEdge[T]]] = target.incomingEdges.groupBy(_.order).toSeq.sortBy(_._1).map(_._2)
        val dataInsOnPorts = dataIns.map { dataInsOnePort =>
          val dataCandidates: Seq[T] = dataInsOnePort.map(edge => edge.impl(signalMap(edge.source), edge.weight))
          val mux = DFGMUX[T](dataInsOnePort.map(_.schedules))
          mux.impl(dataCandidates, globalCounter.value, globalLcm)
        }
        signalMap(target) := target.impl(dataInsOnPorts)
      }
    }

    outputNodes.map(signalMap(_))
  }

  // feasibilityConstraintGraph
  def fcg = {
    val cg = ConstraintGraph[T]
    foreachVertex(cg.addVertex(_))
    println(cg)
    foreachEdge(edge => cg.addConstraint(edge.source - edge.target <= edge.weight))
    cg
  }

  override def toString: String =
    s"nodes:\n${vertexSet().mkString(" ")}\n" +
      s"edges:\n${edgeSet().map(edge => s"${edge.source}->${edge.target}, delay = ${getEdgeWeight(edge)}").mkString("\n")}\n" +
      s"cycles:\n${new alg.cycle.CycleDetector(this).findCycles().mkString(" ")}"
}

object DFG {
  def apply[T <: Data](implicit holderProvider: BitCount => T): DFG[T] = new DFG()
}