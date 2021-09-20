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

class DFG extends DefaultDirectedWeightedGraph[DSPNode, DSPEdge](classOf[DSPEdge]) {

  implicit class EdgeProperties(edge: DSPEdge) {
    def target = getEdgeTarget(edge)

    def source = getEdgeSource(edge)

    def weight = getEdgeWeight(edge)
  }

  implicit class NodeProperties(node: DSPNode) {

    def incomingEdges = incomingEdgesOf(node)

    def outgoingEdges = outgoingEdgesOf(node)

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.map(_.target)

    def isInput = incomingEdgesOf(node).size() == 0

    def isOutput = outgoingEdgesOf(node).size() == 0
  }

  def inputNodes = vertexSet().filter(node => incomingEdgesOf(node).size() == 0)

  def outputNodes = vertexSet().filter(node => outgoingEdgesOf(node).size() == 0)

  val impl = (dataIn: Seq[Bits]) => {
    case class NodeInfo(data: Bits, var done: Boolean)
    val implemented = mutable.Map[DSPNode, NodeInfo]()

    // starts from the inputs
    inputNodes.zip(dataIn).foreach { case (node, bits) => implemented += (node -> NodeInfo(node.transform(bits), false)) }

    // look forward until all nodes are implemented
    def notDone = implemented.filterNot(_._2.done).keys // nodes that have targets not implemented

    while (implemented.size < vertexSet().size()) {
      println(s"all ${vertexSet().size()}, done ${implemented.size}")
      notDone.foreach { source => // view targets of current nodes
        source.targets.filterNot(implemented.containsKey(_)).foreach { target => // view targets that have not been implemented
          if (target.sources.forall(implemented.containsKey(_))) { // if sources are ready, implement the target
            val dataIn = target.sources.map{source =>
              val edge = getEdge(source,target)
              edge.delay(implemented(source).data, edge.weight.toInt)
            }+
            val dataOut = target.transform(Vec(dataIn).asBits)
            implemented += (target -> NodeInfo(dataOut, false)) // and add the target to implemented
          }
        }
        if (source.targets.forall(implemented.containsKey(_))) implemented(source).done = true
        if (source.isOutput) implemented(source).done = true
      }
    }
    val dataOut = implemented.filterKeys(_.isOutput).values.map(_.data).toSeq
    dataOut
  }
}

class Inc() extends DSPNode {

  override def transform(dataIn: Bits): Bits = (dataIn.asUInt + U(1)).asBits

  override def delay: Int = 0

  override def executionTime: Double = 0.2

}

class Regs() extends DSPEdge {
  override def delay(dataIn: Bits, delay: Int): Bits = Delay(dataIn, delay)
}

object DFG {

  def main(args: Array[String]): Unit = {
    val g = new DFG()
    val inc0 = new Inc()
    val inc1 = new Inc()
    val regs0 = new Regs()

    g.addVertex(inc0)
    g.addVertex(inc1)
    g.addEdge(inc0, inc1, regs0)
    g.setEdgeWeight(regs0, 1)

    println(g.inputNodes.mkString(" "))
    println(g.outputNodes.mkString(" "))

    GenRTL(new Component {
      val dataIn = UInt(4 bits)
      val dataOut = UInt(4 bits)
      dataOut := g.impl(Seq(dataIn.asBits)).head.asUInt
    })
  }
}

