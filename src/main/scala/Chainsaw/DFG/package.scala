package Chainsaw

import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._
import xilinx.VivadoReport

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}

// something new

package object DFG {

  type HolderProvider[T] = BitCount => T

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String): DeviceNode[T] = DeviceNode(name, hardware)
  }

  case class ImplPolicy(useRegInit: Boolean, useSubmodule: Boolean, useDontCare: Boolean)

  val testImplPolicy = ImplPolicy(true, false, false)

  val synthImplPolicy = ImplPolicy(false, true, true)

  var globalImplPolicy: ImplPolicy = testImplPolicy // this version is problem-free under current testbench

  //  var globalImplPolicy: ImplPolicy = ImplPolicy(false, true) // this is the config we should take

  // holder providers, serving impl methods
  implicit val sintProvider: HolderProvider[SInt] = (width: BitCount) => if (width.value >= 1) SInt(width) else SInt()
  implicit val uintProvider: HolderProvider[UInt] = (width: BitCount) => if (width.value >= 1) UInt(width) else UInt()
  implicit val bitsProvider: HolderProvider[Bits] = (width: BitCount) => if (width.value >= 1) Bits(width) else Bits()

  implicit val complexProvider: HolderProvider[ComplexNumber] =
    (width: BitCount) => ComplexNumber(1, width.value - 2) // FIXME: we use 1 bit in integral part now

  // soft -> hard converters, serving literal-related methods
  implicit val sintConverter: (Int, BitCount) => SInt = (value: Int, width: BitCount) => S(value, width)
  implicit val uintConverter: (Int, BitCount) => UInt = (value: Int, width: BitCount) => U(value, width)
  implicit val bitsConverter: (Int, BitCount) => Bits = (value: Int, width: BitCount) => B(value, width)

  // graph utils for constructing DFG
  implicit class graphUtils[T <: Data](dfg: DFGGraph[T]) {}

  // node utils for constructing DFG
  implicit class nodeUtils[T <: Data](node: DSPNode[T]) {
    def >=>(delay: Double) = DSPAssignment(node, delay, node)

    def >>(delay: Double) = DSPPath(ArrayBuffer(node), ArrayBuffer(delay))

    def >>(that: DSPNode[T]) = DSPPath(ArrayBuffer(node, that), ArrayBuffer(0))

    /** to form an inequality that a - b <= 0
      */
    def -(that: DSPNode[T]): DSPConstraint[T] = DSPConstraint(node, that, 0)

    /** extend a virtual node from a output port of current node
      */
    def extendVirtual(order: Int): VirtualNode[T] = VirtualNode[T](s"${node}_v", node.outWidths(order))
  }

  implicit class nodesUtils[T <: Data](nodes: Seq[DSPNode[T]]) {
    def >=>(delays: Seq[Double]) = DSPAssignment(nodes, delays, nodes.head)
  }

  /** edge properties in a specific DFG
    */
  implicit class EdgeProperties[T <: Data](edge: DSPEdge[T])(implicit dfg: DFGGraph[T]) {

    def target: DSPNode[T] = dfg.getEdgeTarget(edge)

    def source: DSPNode[T] = dfg.getEdgeSource(edge)

    def isLatencyEdge: Boolean = edge.isInstanceOf[LatencyEdge[T]]

    // weight-related properties

    /** we keep it a Double here as sometimes, DFG will be used/cloned for other purpose(such as critical path calculation)
      */
    def weight: Double = dfg.getEdgeWeight(edge)

    /** delay on the edge
      */
    def delay: Int = weight.toInt

    /** the actual delay through the edge, useful in DFG transformations
      */
    def weightWithSource: Int = (edge.weight + edge.source.delay).toInt

    def symbol = s"$source(${edge.outOrder}) -> ${edge.weightWithSource} -> $target(${edge.inOrder})"
  }

  implicit class NodeProperties[T <: Data](node: DSPNode[T])(implicit dfg: DFGGraph[T]) {

    // properties
    def outgoingEdges: Seq[DSPEdge[T]] = dfg.outgoingEdgesOf(node).toSeq

    def incomingEdges: Seq[DSPEdge[T]] = dfg.incomingEdgesOf(node).toSeq

    def sources: Seq[DSPNode[T]] = incomingEdges.map(_.source)

    def targets: Seq[DSPNode[T]] = outgoingEdges.map(_.target)

    // actions
    def addVertex() = dfg.addVertex(node)

    def addConstantDriver[TSoft](constant: TSoft, width: BitCount, order: Int, schedules: Seq[Schedule] = NoMUX())(implicit
        converter: (TSoft, BitCount) => T
    ): Unit = {
      val constantNode = ConstantNode(s"constant_$constant", constant, width)
      dfg.addVertex(constantNode)
      dfg.addEdge(constantNode(0), node(order), 0, schedules)
    }

    def addConstantDriver(constant: T, order: Int, schedules: Seq[Schedule]): Unit = {
      val constantNode = ConstantNode(s"constant", constant)
      dfg.addVertex(constantNode)
      dfg.addEdge(constantNode(0), node(order), 0, schedules)
    }

  }

  implicit class BinaryNodeWithConst[T <: Data](dfg: DFGGraph[T])(implicit converter: (Int, BitCount) => T) {
    def genConstBinaryNode(node: BinaryNode[T], constant: Int, width: BitCount = 10 bits, order: Int = 0): Unit = {
      val cnode = ConstantNode[T, Int](s"constnode${constant}", constant, width)
      Seq(cnode, node).foreach(dfg.addVertex)
      dfg.addEdge(cnode(0), node(order), 0)
    }

    def genConstTrinaryNode(node: TrinaryNode[T], constant: Int, width: BitCount = 10 bits, order: Int = 0): Unit = {
      val cnode = ConstantNode[T, Int](s"constnode${constant}", constant, width)
      Seq(cnode, node).foreach(dfg.addVertex)
      dfg.addEdge(cnode(0), node(order), 0)
    }

  }

  implicit def defaultOrder[T <: Data](node: DSPNode[T]): DSPNodeWithOrder[T] = DSPNodeWithOrder(node, 0)

  // gen, sim, synth and impl utils of DFGGraph and Nodes

  /** wrap a DSPNode as a component, this is the supportive method for all following tasks
    *
    * @param node
    *   dut node
    * @param inputWidths
    *   as impl is used, this is necessary
    * @param forTiming
    *   when set, extra registers are applied on the input/output ports for more accurate timing statistics
    * @param holderProvider
    *   as impl is used, this is necessary
    * @return
    *   a component which can be tested and synthesized
    */
  def wrappedNode[THard <: Data](node: DSPNode[THard], inputWidths: Seq[BitCount], forTiming: Boolean = false)(implicit
      holderProvider: BitCount => THard
  ): Component with DSPTestable[Vec[THard], Vec[THard]] = {
    new Component with DSPTestable[Vec[THard], Vec[THard]] {
      override val dataIn: Flow[Vec[THard]]  = slave Flow Vec(inputWidths.map(holderProvider(_)))
      override val dataOut: Flow[Vec[THard]] = master Flow Vec(node.outWidths.map(holderProvider(_)))
      override val latency: Int              = node.delay
      if (forTiming) {
        dataOut.valid   := Delay(dataIn.valid, latency + 2, init = False)
        dataOut.payload := RegNext(Vec(node.impl(RegNext(dataIn.payload), GlobalCount(U(0)))))
      } else {
        dataOut.valid   := Delay(dataIn.valid, latency, init = False)
        dataOut.payload := Vec(node.impl(dataIn.payload, GlobalCount(U(0))))
      }
      setDefinitionName(node.name)
    }
  }

  /** generate the RTL code(verilog) of a DSPNode
    *
    * @param node
    * @param inputWidths
    *   input widths which drives all other widths
    */
  def genDSPNode[THard <: Data](node: DSPNode[THard], inputWidths: Seq[BitCount])(implicit holderProvider: BitCount => THard): Unit =
    GenRTL(wrappedNode(node, inputWidths), name = node.name)

  def genDFG[THard <: Data](dfg: DFGGraph[THard], inputWidths: Seq[BitCount])(implicit holderProvider: BitCount => THard): Unit =
    genDSPNode(dfg.asNode(dfg.name, 0 cycles), inputWidths)

  /** test a DSPNode as a data-driven DUT, defined by the input/output
    *
    * @param initLength
    *   cycles for initialization, corresponding results would be dropped
    */
  def testDSPNode[THard <: Data, Si, So](node: DSPNode[THard], inputWidths: Seq[BitCount], testCases: Seq[Si], golden: Seq[So], initLength: Int = 0)(implicit
      holderProvider: BitCount => THard
  ): Seq[Si] =
    doFlowPeekPokeTest(node.name, wrappedNode(node, inputWidths), testCases, golden, initLength).asInstanceOf[Seq[Si]]

  def testDFG[THard <: Data, Si, So](
      dfg: DFGGraph[THard],
      graphLatency: CyclesCount,
      inputWidths: Seq[BitCount],
      testCases: Seq[Si],
      golden: Seq[So],
      initLength: Int = 0
  )(implicit holderProvider: BitCount => THard): Seq[Si] =
    testDSPNode(dfg.asNode(dfg.name, graphLatency), inputWidths, testCases, golden, initLength)

  /** synthesis a DSPNode using Vivado
    */
  def synthDSPNode[THard <: Data](node: DSPNode[THard], inputWidths: Seq[BitCount], forTiming: Boolean = false)(implicit
      holderProvider: BitCount => THard
  ): VivadoReport =
    VivadoSynth(wrappedNode(node, inputWidths, forTiming), name = node.name)

  def synthDFG[THard <: Data](dfg: DFGGraph[THard], inputWidths: Seq[BitCount], forTiming: Boolean = false)(implicit
      holderProvider: BitCount => THard
  ): VivadoReport =
    VivadoSynth(wrappedNode(dfg.asNode(dfg.name), inputWidths, forTiming), dfg.name)

  /** implement(synth, place and route) a DSPNode using Vivado
    */
  def implDSPNode[THard <: Data](node: DSPNode[THard], inputWidths: Seq[BitCount], forTiming: Boolean = false)(implicit
      holderProvider: BitCount => THard
  ): VivadoReport =
    VivadoImpl(wrappedNode(node, inputWidths, forTiming), name = node.name)

  def implDFG[THard <: Data](dfg: DFGGraph[THard], inputWidths: Seq[BitCount], forTiming: Boolean = false)(implicit
      holderProvider: BitCount => THard
  ): VivadoReport =
    VivadoImpl(wrappedNode(dfg.asNode(dfg.name), inputWidths, forTiming), dfg.name)

  // tuple 2 seq, better way?

}
