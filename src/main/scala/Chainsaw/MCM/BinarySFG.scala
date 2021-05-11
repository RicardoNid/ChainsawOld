package Chainsaw.MCM

import org.jgrapht.graph._

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class BinarySFG extends DirectedMultigraph[Int, DefaultEdge](classOf[DefaultEdge]) {

  def size = this.vertexSet().size()

  def apply(n: Int) = this.vertexSet().filter(_ == n).head

  def driversOf(v: Int) = this.incomingEdgesOf(v).toSeq.map(this.getEdgeSource(_))

  def inputs = this.vertexSet().filter(this.inDegreeOf(_) == 0)

  def outputs = this.vertexSet().filter(this.outDegreeOf(_) == 0)

  def addVertex(src0: Int, src1: Int): DefaultEdge = {
    val vertex = this.vertexSet().size()
    this.addVertex(vertex)
    this.addEdge(src0, vertex)
    this.addEdge(src1, vertex)
  }

}

import spinal.core._

class HomogeneousBinarySFGBuilder[T <: Data, I](inputs: Seq[T], sfg: BinarySFG, binaryOperator: ((T, I), (T, I)) => T, infos: Seq[I]) extends ImplicitArea[Seq[T]] {
  require(sfg.vertexSet().size() == infos.length)
  val n = sfg.vertexSet().size()
  val signals = ListBuffer[T]()

  (0 until n).foreach { v =>
    val drivers = sfg.driversOf(v)
    if (drivers.isEmpty) signals += inputs(v)
    else if (drivers.length == 2) {
      val (left, right) = (drivers(0), drivers(1))
      signals += binaryOperator(
        (signals(left), infos(left)),
        (signals(right), infos(right))
      )
    }
    else throw new IllegalArgumentException("inDegree of each vertex should be 2")
  }

  override def implicitValue: Seq[T] = sfg.outputs.map(signals(_)).toSeq
}
object HomogeneousBinarySFGBuilder {

  /** Factory with no infos
   */
  def apply[T <: Data](inputs: Seq[T], sfg: BinarySFG, binaryOperator: (T, T) => T) = {
    val paddedOperator = (left: (T, Null), right: (T, Null)) =>
      binaryOperator(left._1, right._1)
    val virtualInfos = Seq.fill(sfg.size)(null)
    new HomogeneousBinarySFGBuilder(inputs, sfg, paddedOperator, virtualInfos)
  }

  def main(args: Array[String]): Unit = {
    val sfg = new BinarySFG
    sfg.addVertex(0)
    sfg.addVertex(0, 0)
    sfg.addVertex(0, 0)
    sfg.addVertex(1, 2)
    //    val add = (left: SReal, right: SReal) => left + right
    val add = (left: UInt, right: UInt) => left + right
    SpinalConfig().generateSystemVerilog(new Component {
      //      val x = in SReal("x", UnitRange(0.1))
      val x = in UInt (4 bits)
      val graph = HomogeneousBinarySFGBuilder(Seq(x), sfg, add)
      val y = graph.implicitValue.head
      out(y)
    })
  }
}