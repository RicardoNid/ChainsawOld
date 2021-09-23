package Chainsaw.DFG

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import spinal.core

class DFGTest extends AnyFlatSpec {

  behavior of "DFGTest"

  it should "run example 1 successfully" in {
    class Inc() extends DSPNode {

      override def impl(dataIn: Seq[Bits]): Bits = (dataIn.head.asUInt + U(1)).asBits

      override def delay: Int = 0

      override def executionTime: Double = 0.2

    }

    // example1
    val g = new DFG()

    val dataIn = g.addInput()

    val inc1 = new Inc()
    val inc0 = ((dataIn: Seq[Bits]) => (dataIn(0).asUInt + U(1)).asBits)
      .asDSPNode(0, 0.2, name = "inc")
    g.addVertexFromSource(dataIn, inc0, 0)
    g.addVertexFromSource(inc0, inc1, 2)
    g.setOutput(inc1)

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
    h.setOutput(add)

    GenRTL(new Component {
      val dataIn = in UInt (4 bits)
      val dataOut = out UInt (4 bits)
      dataOut := h.implRecursive(Seq(dataIn.asBits)).head.asUInt
    })

  }

  it should "run fig 2.2 successfully" in {
    val dfg = new DFG()
    val Seq(n1, n2, n3) = (1 to 3).map(i => AbstractNode(0, 1, "n" + i.toString))
    val Seq(n4, n5, n6) = (4 to 6).map(i => AbstractNode(0, 2, "n" + i.toString))

    dfg.addVertex(n1)
    Seq(n1, n1, n1).zip(Seq(n4, n5, n6)).zip(Seq(2, 3, 4)).foreach { case ((src, des), delay) => dfg.addVertexFromSource(src, des, delay) }
    Seq(n4, n5, n6).zip(Seq(n2, n3, n3)).zip(Seq(0, 0, 0)).foreach { case ((src, des), delay) => dfg.addVertexFromSource(src, des, delay) }
    Seq(n2, n3).zip(Seq(n1, n2)).zip(Seq(0, 0, 0)).foreach { case ((src, des), delay) => dfg.addVertexFromSource(src, des, delay) }

    println(dfg)
    dfg.mergeDelays()
    println(dfg)

    val x = dfg.addInput()
    dfg.addEdge(x, n1, 0)
    dfg.setOutput(n6)

    assert(dfg.delaysCount == 4 && dfg.latency == 4 && dfg.criticalPathLength == 5.0 && dfg.iterationBound == 2.0)

    println(s"dfg is ${if (dfg.isRecursive) "recursive" else "not recursive"}, it has: " +
      s"\n\t${dfg.delaysCount} delays in total \n\tlatency = ${dfg.latency} " +
      s"\n\tcritical path length = ${dfg.criticalPathLength} \n\titeration bound = ${dfg.iterationBound}")
  }

  it should "shortestDelayPath" in {

  }

  it should "shortestDelayPaths" in {

  }

  it should "mergeDelays" in {

  }

  it should "latency" in {

  }

  it should "delaysCount" in {

  }

  it should "longestZeroDelayPath" in {

  }

}
