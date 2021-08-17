package Chainsaw.Architectures

import Chainsaw.{Real, _}
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ListBuffer

// TODO: sign extension
class WallaceTree(input: Vec[Real]) extends ImplicitArea[Real] with Testable {

  // build the bit table
  val tableMax = input.map(_.maxExp).max
  val tableMin = input.map(_.minExp).min

  println(input.mkString("\n"))
  // alignment
  val table = ListBuffer.fill(tableMax - tableMin)(ListBuffer[Bool]())
  input.foreach(operand =>
    (operand.minExp - tableMin until operand.maxExp - tableMin)
      .foreach(i => table(i) += operand.raw(i)))

  def FA(x: Bool, y: Bool, z: Bool) = (z & (x ^ y) | x & y, x ^ y ^ z) // carry, sum

  def BuildTree(table: ListBuffer[ListBuffer[Bool]]): ListBuffer[ListBuffer[Bool]] = {
    //    println(s"before: ${table.map(_.length).mkString(" ")}")

    def doWallace(col: ListBuffer[Bool]) = {
      val carrys = ListBuffer[Bool]()
      val sums = ListBuffer[Bool]()
      val groupNum = col.length / 3
      val groups = col.grouped(3).toArray
      (0 until groupNum).foreach { i =>
        val (carry, sum) = FA(groups(i)(0), groups(i)(1), groups(i)(2))
        carrys += carry
        sums += sum
      }
      if (groupNum < groups.length) sums ++= groups.last
      (carrys, sums)
    }

    val ret = ListBuffer[ListBuffer[Bool]]()
    table.foreach { col =>
      val (carrys, sums) = doWallace(col)
      if (ret.isEmpty) {
        ret += sums
        ret += carrys
      }
      else {
        ret.last ++= sums
        ret += carrys
      }
    }
    if (ret.last.isEmpty) ret -= ret.last

    println(s"after: ${ret.map(_.length).mkString(" ")}")
    if (ret.forall(_.length < 3)) ret
    else BuildTree(ret)
  }

  import spinal.core.sim._

  val operands = BuildTree(table)
  val operandsLeft = operands.map(lb => lb(0)).asBits().asUInt
  val operandsRight = operands.map(lb => if (lb.length == 2) lb(1) else False).asBits().asUInt
  operandsLeft.simPublic()
  operandsRight.simPublic()

  // TODO: find a better range
  val ret = Real(input.map(_.lower).sum, input.map(_.upper).sum, input.map(_.minExp).min exp)
  println(ret)
  ret.raw := (operandsLeft +^ operandsRight).intoSInt.resized // TODO: need to be shifted

  override def implicitValue: Real = RegNext(ret)

  override val getTimingInfo = TimingInfo(1, 1, 1, 2)
}

object WallaceTree {

  def apply(input: Vec[Real]): WallaceTree = new WallaceTree(input)

  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new Component {
      //      val input = in Vec (3 until 8).map(i => Real(IntRange(0, (1 << i) - 1)))
      val input = in Vec(IntReal(0, 63), 7)
      val output = new WallaceTree(input).implicitValue
      out(output)
    })
  }
}