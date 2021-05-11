package Chainsaw.MCM

import spinal.core._

import scala.collection.mutable.ListBuffer

class HomogeneousBinarySFGBuilder[T <: Data, I](inputs: Seq[T], sfg: BinarySFG, binaryOperator: (T, T, I) => T, infos: Seq[I]) extends ImplicitArea[Seq[T]] {
  require(sfg.vertexSet().size() == infos.length)
  val n = sfg.vertexSet().size()
  val signals = ListBuffer[T]()

  (0 until n).foreach { v =>
    val drivers = sfg.driversOf(v)
    if (drivers.isEmpty) signals += inputs(v)
    else if (drivers.length == 2) {
      val (left, right) = (drivers(0), drivers(1))
      signals += binaryOperator(signals(left), signals(right), infos(v))
    }
    else throw new IllegalArgumentException("inDegree of each vertex should be 2")
  }

  override def implicitValue: Seq[T] = sfg.outputs.map(signals(_)).toSeq
}
object HomogeneousBinarySFGBuilder {

  /** Factory with no infos
   */
  def apply[T <: Data](inputs: Seq[T], sfg: BinarySFG, binaryOperator: (T, T) => T) = {
    val paddedOperator = (left: T, right: T, info: Null) =>
      binaryOperator(left, right)
    val virtualInfos = Seq.fill(sfg.size)(null)
    new HomogeneousBinarySFGBuilder(inputs, sfg, paddedOperator, virtualInfos)
  }

  def main(args: Array[String]): Unit = {

    val sfg = new BinarySFG
    sfg.addVertex(0)
    sfg.addVertex(0, 0)
    sfg.addVertex(0, 0)
    sfg.addVertex(1, 2)

    // example of addition
    val add = (left: UInt, right: UInt) => left + right
    SpinalConfig().generateSystemVerilog(new Component {
      //      val x = in SReal("x", UnitRange(0.1))
      val x = in UInt (4 bits)
      val graph = HomogeneousBinarySFGBuilder(Seq(x), sfg, add)
      val y = graph.implicitValue.head
      out(y)
    })

    // example of MAG
    import AOpSign._
    import AOperations._
    val infos = Seq(AOpConfig(1, 0, 0, ADD), AOpConfig(1, 0, 0, ADD), AOpConfig(2, 0, 0, ADD), AOpConfig(3, 0, 0, ADD))
    SpinalConfig().generateSystemVerilog(new Component {
      val x = in SReal("x", UnitRange(0.1))
      val graph = new HomogeneousBinarySFGBuilder(Seq(x), sfg, AOpHardware, infos)
      val y = graph.implicitValue.head
      out(y)
    })
  }
}