package Chainsaw

import Chainsaw.DFG.{DSPEdge, DSPNode}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht.GraphPath

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._

import scala.collection.mutable.ArrayBuffer

package object DFG {

  implicit val sintProvider = (width: BitCount) => if (width.value >= 1) SInt(width) else SInt()
  implicit val uintProvider = (width: BitCount) => if (width.value >= 1) UInt(width) else UInt()
  implicit val bitsProvider = (width: BitCount) => if (width.value >= 1) Bits(width) else Bits()
  implicit val complexProvider = (width: BitCount) => ComplexNumber(1, width.value - 2)

  implicit class nodeUtils[T <: Data](node: DSPNode[T]) {
    def >=>(delay: Double) = DSPAssignment(node, delay, node)

    def >>(delay: Double) = DSPPath(ArrayBuffer(node), ArrayBuffer(delay)) // TODO: more elegant implementation

    def -(that: DSPNode[T]) = DSPConstraint(node, that, 0)

    def isIO = node.isInstanceOf[InputNode[T]] || node.isInstanceOf[OutputNode[T]]
  }

  implicit class nodesUtils[T <: Data](nodes: Seq[DSPNode[T]]) {
    def >=>(delays: Seq[Double]) = DSPAssignment(nodes, delays, nodes.head)
  }

  implicit class EdgeProperties[T <: Data](edge: DSPEdge[T])(implicit dfg: DFGGraph[T]) {
    def target = dfg.getEdgeTarget(edge)

    def source = dfg.getEdgeSource(edge)

    def weight = dfg.getEdgeWeight(edge)

    def symbol = s"$source(${edge.outOrder}) -> ${edge.weight} -> $target(${edge.inOrder})"
  }

  implicit class NodeProperties[T <: Data](node: DSPNode[T])(implicit dfg: DFGGraph[T]) {

    def outgoingEdges = dfg.outgoingEdgesOf(node).toSeq

    def incomingEdges = dfg.incomingEdgesOf(node).toSeq

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.map(_.target)

  }

}
