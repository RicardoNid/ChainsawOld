package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import breeze.linalg._
import breeze.math._
import spinal.core._

object DFGDSL {

  class SIntNodeRing(implicit graph: DFGGraph[SInt]) extends Semiring[DFGNode[SInt]] {

    val Hardware = BinaryHardware(sintMult, 10 bits, 0 cycles, 1 ns)

    override def zero = new DFGNode("zero", Hardware)

    override def one = new DFGNode("one", Hardware)

    override def +(a: DFGNode[SInt], b: DFGNode[SInt]) = {
      val ret = new DFGNode("sum", Hardware)
      graph.addEdge(a, ret, 0)
      graph.addEdge(b, ret, 0)
      ret
    }

    override def *(a: DFGNode[SInt], b: DFGNode[SInt]) = {
      val ret = new DFGNode("product", Hardware)
      graph.addEdge(a, ret, 0)
      graph.addEdge(b, ret, 0)
      ret
    }

    override def ==(a: DFGNode[SInt], b: DFGNode[SInt]) = true

    override def !=(a: DFGNode[SInt], b: DFGNode[SInt]) = true
  }

  class DFGArea[T <: Data](name:String) {
    implicit val dfg = DFGGraph[T](name)
  }

  class DFGNode[T <: Data](override val name:String, override val hardware: DSPHardware[T])(implicit dfg:DFGGraph[T])
    extends DSPNode[T](name, hardware){
    dfg.addVertex(this)
  }

  def main(args: Array[String]): Unit = {

    val area = new DFGArea[SInt]("add"){
      implicit val sintRing = new SIntNodeRing()
      val uintHardware = BinaryHardware(sintMult, 10 bits, 0 cycles, 1 ns)
      val x = new DFGNode("x", uintHardware)
      val y = new DFGNode("y", uintHardware)
      val z = x * y
    }

    println(area.dfg)
  }

}
