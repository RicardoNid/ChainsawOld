package Chainsaw.tobeTransplanted

import Chainsaw.Real
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
    // example of MAG, using MAG algo
    val (mag, magInfos) = MAG(41623)
    import AOperations.AOpHardware
    println(mag)
    println(magInfos.mkString("\n"))
    SpinalConfig().generateSystemVerilog(new Component {
      val x = in(Real(-1.0, 1.0, 0.1))
      val graph = new HomogeneousBinarySFGBuilder(Seq(x), mag, AOpHardware, magInfos)
      val y = graph.implicitValue.head
      out(y)
    })
  }
}