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
    def addConstBinaryNode(number: Int, constexpre: Int => Int, op: (T, T) => T, opname: String = "opnode", widthC: BitCount = -1 bits, delayC: CyclesCount = 0 cycles , exeTimeC: TimeNumber = 1 ns,  order: Int = 0):Seq[BinaryNode[T]] = {
        val opnodes = (0 until number).map{i => BinaryNode(op, s"${opname}${i + 1}", widthC, delayC, exeTimeC)}
        if((0 until number).map(constexpre(_)).distinct.size == 1) {
          val cnode = ConstantNode[T , Int](s"cnode0", constexpre(0), widthC)
          (opnodes :+ cnode).foreach(dfg.addVertex(_))
          opnodes.foreach(opnode => dfg.addEdge(cnode(0), opnode(order), 0))
          opnodes
        }
        else {
          val cnodes = (0 until number).map{i => ConstantNode[T , Int](s"cnode${i + 1}", constexpre(i), widthC)}
          (cnodes ++ opnodes).foreach(dfg.addVertex(_))
          cnodes.zip(opnodes).foreach{ case(cnode, opnode) => dfg.addEdge(cnode(0), opnode(order), 0)}
          opnodes
        }
    }

    def addBinaryNodes(ops: Seq[((T, T) => T, Int)], names: Seq[String], widths: BitCount = -1 bits, delays: CyclesCount = 0 cycles, exeTimes: TimeNumber = 1 ns):Seq[Seq[BinaryNode[T]]] = {
      val result = ops.zip(names).map{ case(op, name) =>
        val opnodes = (0 until op._2).map(i => BinaryNode(op._1, s"${name}${i + 1}", widths, delays, exeTimes))
        opnodes.foreach(dfg.addVertex(_))
        opnodes.toSeq
      }
      result
    }

    def addConstTrinaryNode(number: Int, constexpre: Int => Int, op: (T, T, T) => T, opname: String = "opnode", widthC: BitCount = -1 bits, delayC: CyclesCount = 0 cycles , exeTimeC: TimeNumber = 1 ns,  order: Int = 0):Seq[TrinaryNode[T]] = {
      val opnodes = (0 until number).map{i => TrinaryNode(op, s"${opname}${i + 1}", widthC, delayC, exeTimeC)}
      if((0 until number).map(constexpre(_)).distinct.size == 1) {
        val cnode = ConstantNode[T , Int](s"cnode0", constexpre(0), widthC)
        (opnodes :+ cnode).foreach(dfg.addVertex(_))
        opnodes.foreach(opnode => dfg.addEdge(cnode(0), opnode(order), 0))
        opnodes
      }
      else {
        val cnodes = (0 until number).map{i => ConstantNode[T , Int](s"cnode${i + 1}", constexpre(i), widthC)}
        (cnodes ++ opnodes).foreach(dfg.addVertex(_))
        cnodes.zip(opnodes).foreach{ case(cnode, opnode) => dfg.addEdge(cnode(0), opnode(order), 0)}
        opnodes
      }
    }

    def addTrinaryNodes(ops: Seq[((T, T, T) => T, Int)], names: Seq[String], widths: BitCount = -1 bits, delays: CyclesCount = 0 cycles, exeTimes: TimeNumber = 1 ns):Seq[Seq[TrinaryNode[T]]] = {
      val result = ops.zip(names).map{ case(op, name) =>
        val opnodes = (0 until op._2).map(i => TrinaryNode(op._1, s"${name}${i + 1}", widths, delays, exeTimes))
        opnodes.foreach(dfg.addVertex(_))
        opnodes
      }
      result
    }
  }

  implicit def defaultOrder[T](node: DSPNode[T]): DSPNodeWithOrder[T] = DSPNodeWithOrder(node, 0)

  // TODO: better type inference
  def testDSPNode[THard <: Data, Si, So](node: DSPNode[THard], inputWidths: Seq[BitCount], testCases: Seq[Si], golden: Seq[So])
                                    (implicit holderProvider: BitCount => THard): Seq[Si] = {
    doFlowPeekPokeTest(node.name, new Component with DSPTestable[Vec[THard], Vec[THard]] {
      override val dataIn: Flow[Vec[THard]] = slave Flow Vec(inputWidths.map(holderProvider(_)))
      override val dataOut: Flow[Vec[THard]] = master Flow Vec(node.hardware.outWidths.map(holderProvider(_)))
      override val latency: Int = node.delay

      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload := Vec(node.hardware.impl(dataIn.payload, GlobalCount(U(0))))

    }, testCases, golden).asInstanceOf[Seq[Si]]
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

}
