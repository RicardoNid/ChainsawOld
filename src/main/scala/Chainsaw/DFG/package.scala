package Chainsaw

import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

package object DFG {

  implicit val sintProvider = (width: BitCount) => if (width.value >= 1) SInt(width) else SInt()
  implicit val uintProvider = (width: BitCount) => if (width.value >= 1) UInt(width) else UInt()
  implicit val bitsProvider = (width: BitCount) => if (width.value >= 1) Bits(width) else Bits()
  implicit val complexProvider = (width: BitCount) => ComplexNumber(1, width.value - 2)

  implicit class nodeUtils[T <: Data](node: DSPNode[T]) {
    def >=>(delay: Double) = DSPAssignment(node, delay, node)

    def >>(delay: Double) = DSPPath(ArrayBuffer(node), ArrayBuffer(delay))

    def >>(that: DSPNode[T]) = DSPPath(ArrayBuffer(node, that), ArrayBuffer(0))

    def -(that: DSPNode[T]) = DSPConstraint(node, that, 0)

    def isIO = node.isInstanceOf[InputNode[T]] || node.isInstanceOf[ConstantNode[T]] || node.isInstanceOf[OutputNode[T]]

    def isInput = node.isInstanceOf[InputNode[T]] || node.isInstanceOf[ConstantNode[T]]

    def isOutput = node.isInstanceOf[OutputNode[T]]

    def apply(order: Int) = DSPNodeWithOrder(node, order)

    /** Extend a virtual node from a output port of current node
     *
     * @param order
     */
    def extendVirtual(order: Int) = GeneralNode[T](s"${node}_v", 0 cycles, 0 ns, node.hardware.outWidths(order))

  }

  implicit class nodesUtils[T <: Data](nodes: Seq[DSPNode[T]]) {
    def >=>(delays: Seq[Double]) = DSPAssignment(nodes, delays, nodes.head)
  }

  implicit class EdgeProperties[T <: Data](edge: DSPEdge[T])(implicit dfg: DFGGraph[T]) {
    def target = dfg.getEdgeTarget(edge)

    def source = dfg.getEdgeSource(edge)

    def weight = dfg.getEdgeWeight(edge)

    def weightWithSource = (edge.weight + edge.source.delay).toInt

    def symbol = s"$source(${edge.outOrder}) -> ${edge.weightWithSource} -> $target(${edge.inOrder})"

    def hasNoMux = edge.schedules.size == 1 && edge.schedules.head == Schedule(0, 1)

    /** Get a new edge with different schedules
     */
    def withSchedules(schedules: Seq[Schedule]) = DefaultDelay[T](schedules, edge.outOrder, edge.inOrder)
  }

  implicit class NodeProperties[T <: Data](node: DSPNode[T])(implicit dfg: DFGGraph[T]) {

    def outgoingEdges = dfg.outgoingEdgesOf(node).toSeq

    def incomingEdges = dfg.incomingEdgesOf(node).toSeq

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.map(_.target)

  }

  implicit def defaultOrder[T <: Data](node: DSPNode[T]): DSPNodeWithOrder[T] = DSPNodeWithOrder(node, 0)

}
