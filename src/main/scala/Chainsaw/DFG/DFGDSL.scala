package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import breeze.linalg._
import breeze.math._
import spinal.core._

object DFGDSL {

  class SIntNodeRing(implicit graph: DFGGraph[SInt]) extends Semiring[DSPNode[SInt]] {

    val Hardware = BinaryHardware(sintMult, 10 bits, 0 cycles, 1 ns)

    override def zero = new DSPNode("zero", Hardware)

    override def one = new DSPNode("one", Hardware)

    override def +(a: DSPNode[SInt], b: DSPNode[SInt]) = {
      val ret = new DSPNode(s"${a}_${b}_sum", Hardware)
      graph.addVertices(a, b, ret)
      graph.addEdge(a, ret, 0)
      graph.addEdge(b, ret, 0)
      ret
    }

    override def *(a: DSPNode[SInt], b: DSPNode[SInt]) = {
      val ret = new DSPNode(s"${a}_${b}_product", Hardware)
      graph.addVertices(a, b, ret)
      graph.addEdge(a, ret, 0)
      graph.addEdge(b, ret, 0)
      ret
    }

    override def ==(a: DSPNode[SInt], b: DSPNode[SInt]) = true

    override def !=(a: DSPNode[SInt], b: DSPNode[SInt]) = true
  }

  abstract class DFGArea[T <: Data](name: String) {
    implicit val dfg = DFGGraph[T](name)
    implicit val semiRing: Semiring[DSPNode[T]]
    val node = passThrough[T]()

    def dfgIn(node: DSPNode[T]) = dfg.setInput(node)

    def dfgOut(node: DSPNode[T]) = dfg.setOutput(node)

  }


  def main(args: Array[String]): Unit = {

    val area = new DFGArea[SInt]("add") {

      override implicit val semiRing: Semiring[DSPNode[SInt]] = new SIntNodeRing()

      val x = DenseVector.tabulate(10)(i => new DSPNode(s"x_$i", node))
      val y = DenseVector.tabulate(10)(i => new DSPNode(s"y_$i", node))

//      val z0 = x * y
//      val z1 = x + y
      val z2 = sum(x *:* y)
    }

    println(area.dfg)
  }

}
