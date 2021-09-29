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

import scala.math.min

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DFG extends DefaultDirectedWeightedGraph[DSPNode, DSPEdge](classOf[DSPEdge]) {

  def inputNodes = vertexSet().filter(_.isInstanceOf[InputNode])

  def outputNodes = vertexSet().filter(_.isInstanceOf[OutputNode])

  implicit class EdgeProperties(edge: DSPEdge) {
    def target = getEdgeTarget(edge)

    def source = getEdgeSource(edge)

    def weight = getEdgeWeight(edge).toInt
  }

  implicit class NodeProperties(node: DSPNode) {

    def outgoingEdges = outgoingEdgesOf(node).toSeq

    def incomingEdges = incomingEdgesOf(node).toSeq

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.toSeq.map(_.target)

  }

  def weights = edgeSet().toSeq.map(_.weight)

  def executionTimes = vertexSet().toSeq.map(_.executionTime)

  def foreachVertex(body: DSPNode => Unit) = vertexSet().foreach(body)

  def foreachEdge(body: DSPEdge => Unit) = edgeSet().foreach(body)

  def maxExecutionTime = vertexSet().map(_.executionTime).max

  def addEdge(sourceVertex: DSPNode, targetVertex: DSPNode, delay: Int, order: Int): Boolean = {
    val edge = FPGADelay(order) // temporarily, using FPGADelay as default
    val success = addEdge(sourceVertex, targetVertex, edge)
    setEdgeWeight(edge, delay)
    success
  }

  def addEdge(sourceVertex: DSPNode, targetVertex: DSPNode, delay: Int): Boolean = {
    val defaultOrder = targetVertex.sources.distinct.size
    addEdge(sourceVertex, targetVertex, delay, defaultOrder: Int)
  }

  def addVertices(sources: Seq[DSPNode]): Unit = sources.foreach(addVertex(_))
  //    def addVertices(sources: DSPNode*): Unit = addVertices(sources)

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

  def setInput(target: DSPNode) = {
    val inputNode = InputNode()
    addVertex(inputNode)
    addEdge(inputNode, target, 0)
  }

  def setOutput(source: DSPNode): OutputNode = {
    val outputNode = OutputNode()
    addVertexFromSource(source, outputNode, 0)
    outputNode
  }

  // algorithm objects
  def bellman = new alg.shortestpath.BellmanFordShortestPath(this)

  def cycleDet = new alg.cycle.CycleDetector(this)

  def shortestPath(source: DSPNode, sink: DSPNode): GraphPath[DSPNode, DSPEdge] = bellman.getPath(source, sink) // this may be null

  // properties of DFG
  def isRecursive: Boolean = cycleDet.detectCycles()

  def delaysCount = edgeSet().toSeq.map(_.weight).sum.toInt

  // TODO: consider MIMO system
  def latency = {
    val graph = this.clone().asInstanceOf[DFG]
    graph.edgeSet().foreach(edge => graph.setEdgeWeight(edge, edge.weight + edge.source.delay))
    graph.shortestPath(inputNodes.head, outputNodes.head).getWeight.toInt
  }

  def criticalPathLength = {
    val cpg = criticalPathGraph
    (cpg.edgeSet().map(cpg.getEdgeWeight(_)).max) max (vertexSet().map(_.executionTime).max)
  }

  def graphForCriticalPath = {
    val graph = this.clone().asInstanceOf[DFG]
    graph.edgeSet().filter(_.weight > 0).foreach(graph.removeEdge(_))
    graph.edgeSet().foreach(edge => graph.setEdgeWeight(edge, -edge.target.executionTime))
    graph
  }

  def longestZeroDelayPath(sourceEdge: DSPEdge, targetEdge: DSPEdge) =
    graphForCriticalPath.shortestPath(sourceEdge.target, targetEdge.source)


  def longestExecutionTime(sourceEdge: DSPEdge, targetEdge: DSPEdge) = {
    val path = longestZeroDelayPath(sourceEdge, targetEdge)
    if (path == null) -1 else -path.getWeight + sourceEdge.target.executionTime
  }

  def criticalPathGraph = {
    val vertices = edgeSet().toSeq.filter(_.weight > 0) // use edge with non-zero delay as vertices
    val cpg = new DefaultDirectedWeightedGraph[DSPEdge, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    vertices.foreach(edge => cpg.addVertex(edge))
    Array.tabulate(vertices.size, vertices.size) { (i, j) =>
      val weight = longestExecutionTime(vertices(i), vertices(j))
      if (weight >= 0) cpg.setEdgeWeight(cpg.addEdge(vertices(i), vertices(j)), weight)
    }
    // expand nodes with delay > 1
    cpg.vertexSet().filter(_.weight > 1).foreach { node =>
      // keep the original node as the beginning of the chain
      val others = Seq.fill(node.weight.toInt - 1)(FPGADelay(0))
      others.foreach(cpg.addVertex(_))

      // reconnect the outgoing edges to the last node of the chain
      cpg.outgoingEdgesOf(node).foreach { edge =>
        //        printlnGreen(s"${cpg.getEdgeSource(edge)} -> ${cpg.getEdgeTarget(edge)}")
        val target = cpg.getEdgeTarget(edge)
        val weight = cpg.getEdgeWeight(edge)
        cpg.setEdgeWeight(cpg.addEdge(others.last, target), weight)
        //        printlnGreen(s"${others.last} -> ${target}")
      }
      cpg.removeAllEdges(cpg.outgoingEdgesOf(node).toSeq) // TODO: when using original set, the last edge won't be removed, find out the reason

      // build the chain
      val all: Seq[DSPEdge] = node +: others
      all.init.zip(all.tail).foreach { case (prev, next) => cpg.setEdgeWeight(cpg.addEdge(prev, next), 0) }
    }
    cpg
  }

  def iterationBound = {
    val negatedCPG = criticalPathGraph
    negatedCPG.edgeSet().foreach(edge => negatedCPG.setEdgeWeight(edge, -negatedCPG.getEdgeWeight(edge)))
    val mcm = new alg.cycle.HowardMinimumMeanCycle(negatedCPG)
    -mcm.getCycleMean
  }

  /** Replace an incoming edge while keep the
   *
   * @param oldEdge   the edge to be replaced
   * @param newSource source of the new edge
   * @param newDelay  weight(delay) of the new edge
   */
  def replaceIncomingEdge(oldEdge: DSPEdge, newSource: DSPNode, newDelay: Int) = {
    val edgeBuffer = ArrayBuffer[(DSPNode, Int)]()
    val target = oldEdge.target

    def lastIncomingEdge = target.incomingEdges.last
    // remove
    while (lastIncomingEdge != oldEdge) {
      edgeBuffer += Tuple2(lastIncomingEdge.source, lastIncomingEdge.weight.toInt)
      removeEdge(lastIncomingEdge)
    }
    // replace
    removeEdge(oldEdge)
    addEdge(newSource, target, newDelay)
    // re-add in order
    while (edgeBuffer.nonEmpty) {
      val info = edgeBuffer.last
      addEdge(info._1, target, info._2)
      edgeBuffer.remove(edgeBuffer.size - 1)
    }
  }

  // TODO: find a better solution
  def mergeDelays() = {
    vertexSet().filter(_.outgoingEdges.toSeq.map(_.weight.toInt).filter(_ > 0).size > 1).foreach { v => // for those vertices which drives multiple targets with delays > 0
      val candidates = v.outgoingEdges.toSeq.filter(_.weight.toInt > 0).sortBy(_.weight.toInt) // list delays > 0 in ascending order
      val relativeDelays = candidates.head.weight +: candidates.init.map(_.weight).zip(candidates.tail.map(_.weight)).map { case (prev, next) => next - prev }
      var anchor = v
      candidates.zip(relativeDelays).foreach { case (edge, delay) =>
        val tmp = TmpNode()
        addVertex(tmp)
        addEdge(anchor, tmp, delay.toInt)
        replaceIncomingEdge(edge, tmp, 0)
        anchor = tmp
      }
    }
  }

  def feasibilityConstraintGraph: ConstraintGraph = {
    val cg = ConstraintGraph()
    foreachVertex(cg.addVertex(_))
    foreachEdge(edge => cg.add(edge.source - edge.target <= edge.weight))
    cg
  }

  def applyRetiming(retiming: Seq[Int]) = {
    val r: Map[DSPNode, Int] = vertexSet().toSeq.zip(retiming).map { case (node, i) => node -> i }.toMap
    foreachEdge { edge =>
      val weightAfterRetiming = edge.weight + r(edge.target) - r(edge.source)
      setEdgeWeight(edge, weightAfterRetiming)
    }
  }

  def impl = (dataIn: Seq[Bits]) => {
    if (isRecursive) implRecursive(dataIn)
    else implForward(dataIn)
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
    printlnGreen("init input over")

    // look forward until all nodes are implemented
    def tobeViewed = implemented.filter(!_._2).keys // nodes that have targets not implemented

    while (implemented.size < vertexSet().size()) {
      println(s"total ${vertexSet().size()}, implemented ${implemented.size}")
      tobeViewed.foreach { source => // view targets of current nodes
        source.targets.filterNot(implemented.containsKey(_)).foreach { target => // view targets that have not been implemented
          val dataIn: Seq[Bits] = target.sources.map { source =>
            val edge = getEdge(source, target)
            edge.impl(data(source), edge.weight.toInt)
          }
          data(target) := target.impl(dataIn)
          implemented += (target -> target.isInstanceOf[OutputNode]) // add the target to implemented, output node need not to be viewed
        }
        implemented(source) = true // now the source has been viewed
      }
    }
    val dataOut = outputNodes.map(data(_))
    dataOut
  }

  val implNew = (dataIns: Seq[Bits]) => {
    val signalMap: Map[DSPNode, Bits] = vertexSet().map(node => // a map to connet nodes with their anchors
      if (node.implWidth == -1) node -> Bits()
      else node -> Bits(node.implWidth bits)).toMap

    // create the global counter
    val globalLcm = edgeSet().map(_.schedules).flatten.map(_.period).reduce(lcm(_, _))
    val globalCounter = CounterFreeRun(globalLcm)

    inputNodes.zip(dataIns).foreach { case (node, bits) => signalMap(node) := bits }

    vertexSet().diff(inputNodes).foreach { target =>
      if (!target.isInstanceOf[InputNode]) {
        val dataIns: Seq[Seq[DSPEdge]] = target.incomingEdges.groupBy(_.order).toSeq.sortBy(_._1).map(_._2)
        val dataInsOnPorts = dataIns.map { dataInsOnePort =>
          val dataCandidates: Seq[Bits] = dataInsOnePort.map(edge => edge.impl(signalMap(edge.source), edge.weight))
          val mux = DFGMUX(dataInsOnePort.map(_.schedules))
          mux.impl(dataCandidates, globalCounter.value, globalLcm)
        }
        signalMap(target) := target.impl(dataInsOnPorts)
      }
    }

    outputNodes.map(signalMap(_))
  }
}

object DFG {
  def apply(): DFG = new DFG()
}