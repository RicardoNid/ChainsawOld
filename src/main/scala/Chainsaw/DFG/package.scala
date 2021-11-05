package Chainsaw

import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

// something new

package object DFG {

  implicit val sintProvider = (width: BitCount) => if (width.value >= 1) SInt(width) else SInt()
  implicit val uintProvider = (width: BitCount) => if (width.value >= 1) UInt(width) else UInt()
  implicit val bitsProvider = (width: BitCount) => if (width.value >= 1) Bits(width) else Bits()
  implicit val complexProvider = (width: BitCount) => ComplexNumber(1, width.value - 2) // FIXME: we use 1 bit in integral part now

  implicit val sintConverter: (Int, BitCount) => SInt = (value: Int, width: BitCount) => S(value, width)
  implicit val uintConverter: (Int, BitCount) => UInt = (value: Int, width: BitCount) => U(value, width)
  implicit val bitsConverter: (Int, BitCount) => Bits = (value: Int, width: BitCount) => B(value, width)

  implicit class nodeUtils[T](node: DSPNode[T]) {
    def >=>(delay: Double) = DSPAssignment(node, delay, node)

    def >>(delay: Double) = DSPPath(ArrayBuffer(node), ArrayBuffer(delay))

    def >>(that: DSPNode[T]) = DSPPath(ArrayBuffer(node, that), ArrayBuffer(0))

    def -(that: DSPNode[T]) = DSPConstraint(node, that, 0)

    /** Pointing out whether a node is I/O or not, this is necessary as I/O are treated differently is most algos
     *
     * Caution: constant nodes are also inputs
     */
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

  implicit class nodesUtils[T](nodes: Seq[DSPNode[T]]) {
    def >=>(delays: Seq[Double]) = DSPAssignment(nodes, delays, nodes.head)
  }

  implicit class EdgeProperties[T](edge: DSPEdge[T])(implicit dfg: DFGGraph[T]) {
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

  implicit class NodeProperties[T](node: DSPNode[T])(implicit dfg: DFGGraph[T]) {

    def outgoingEdges = dfg.outgoingEdgesOf(node).toSeq

    def incomingEdges = dfg.incomingEdgesOf(node).toSeq

    def sources = incomingEdges.map(_.source)

    def targets = outgoingEdges.map(_.target)

  }

  implicit class BinaryNodeWithConst[T](dfg: DFGGraph[T])(implicit converter: (Int, BitCount) => T) {
    def genConstBinaryNode(node: BinaryNode[T], constant: Int, width: BitCount = 10 bits, order: Int = 0): Unit = {
      val cnode = ConstantNode[T, Int](s"constnode${constant}", constant, width)

      Seq(cnode, node).foreach(dfg.addVertex(_))
      dfg.addEdge(cnode(0), node(order), 0)
    }

    def genConstTrinaryNode(node: TrinaryNode[T], constant: Int, width: BitCount = 10 bits, order: Int = 0): Unit = {
      val cnode = ConstantNode[T, Int](s"constnode${constant}", constant, width)
      Seq(cnode, node).foreach(dfg.addVertex(_))
      dfg.addEdge(cnode(0), node(order), 0)
    }

  }

  implicit def defaultOrder[T](node: DSPNode[T]): DSPNodeWithOrder[T] = DSPNodeWithOrder(node, 0)

  //  sim and synth utils of DFGGraph and Nodes
  /**
   * @param node dut node
   * @param inputWidths as impl is used, this is necessary
   * @param forTiming when set, extra registers applied on the input/output port, leading to a more accurate timing result
   * @param holderProvider as impl is used, this is necessary
   * @return a component which can be tested and synthesized
   */
  def wrappedNode[THard <: Data](node: DSPNode[THard], inputWidths: Seq[BitCount],
                                         forTiming: Boolean = false)
                                        (implicit holderProvider: BitCount => THard):
  Component with DSPTestable[Vec[THard], Vec[THard]] = {
    new Component with DSPTestable[Vec[THard], Vec[THard]] {
      override val dataIn: Flow[Vec[THard]] = slave Flow Vec(inputWidths.map(holderProvider(_)))
      override val dataOut: Flow[Vec[THard]] = master Flow Vec(node.hardware.outWidths.map(holderProvider(_)))
      override val latency: Int = node.delay
      if (forTiming) {
        dataOut.valid := Delay(dataIn.valid, latency + 2, init = False)
        dataOut.payload := RegNext(Vec(node.hardware.impl(RegNext(dataIn.payload), GlobalCount(U(0)))))
      } else {
        dataOut.valid := Delay(dataIn.valid, latency, init = False)
        dataOut.payload := Vec(node.hardware.impl(dataIn.payload, GlobalCount(U(0))))
      }
    }
  }

  // TODO: better type inference
  def testDSPNode[THard <: Data, Si, So](node: DSPNode[THard], inputWidths: Seq[BitCount], testCases: Seq[Si], golden: Seq[So], initLength: Int = 0)
                                        (implicit holderProvider: BitCount => THard): Seq[Si] = {
    doFlowPeekPokeTest(node.name, new Component with DSPTestable[Vec[THard], Vec[THard]] {
      override val dataIn: Flow[Vec[THard]] = slave Flow Vec(inputWidths.map(holderProvider(_)))
      override val dataOut: Flow[Vec[THard]] = master Flow Vec(node.hardware.outWidths.map(holderProvider(_)))
      override val latency: Int = node.delay

      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload := Vec(node.hardware.impl(dataIn.payload, GlobalCount(U(0))))

    }, testCases, golden, initLength).asInstanceOf[Seq[Si]]
  }

  // TODO: better type inference
  def synthDSPNode[THard <: Data](node: DSPNode[THard], inputWidths: Seq[BitCount])
                                 (implicit holderProvider: BitCount => THard) = {
    VivadoSynth(new Component with DSPTestable[Vec[THard], Vec[THard]] {
      override val dataIn: Flow[Vec[THard]] = slave Flow Vec(inputWidths.map(holderProvider(_)))
      override val dataOut: Flow[Vec[THard]] = master Flow Vec(node.hardware.outWidths.map(holderProvider(_)))
      override val latency: Int = node.delay

      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload := Vec(node.hardware.impl(dataIn.payload, GlobalCount(U(0))))

    }, name = node.name)
  }

  def implDSPNode[THard <: Data](node: DSPNode[THard], inputWidths: Seq[BitCount], forTiming: Boolean = false)
                                (implicit holderProvider: BitCount => THard) = {
    VivadoImpl(wrappedNode(node, inputWidths, forTiming), name = node.name)
  }

}
