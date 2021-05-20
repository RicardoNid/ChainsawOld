package Chainsaw.FloPoCo.Transplanted

import Chainsaw.Architectures.HomogeneousBinarySFGBuilder
import Chainsaw.FloPoCo.flopocoPath
import Chainsaw.MCM.AOperations.AOpHardware
import Chainsaw.MCM.{AOpConfig, AOperations, MAG}
import Chainsaw._
import spinal.core._

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.sys.process.Process

class SCM(input: Real, constant: Int) extends ImplicitArea[Real] with Testable {
  // invoke flopoco and get source
  val command = s"IntConstMult wIn=${input.bitCount} n=$constant"
  Process(s"./flopoco $command", new java.io.File(flopocoPath)) !
  //  val rtl = Source.fromFile(flopocoPath + "/flopoco.vhdl")
  val rtl = Source.fromFile(flopocoPath + "/flopoco.vhdl").getLines()

  // extract the design from the source
  val patternShiftAdd = "([P|M]?[0-9]*)X <-  ([P|M]?[0-9]*)X<<([0-9]+)  \\+ ([P|M]?[0-9]*)X".r.unanchored
  val opStrings = rtl.filter(line => patternShiftAdd.findAllIn(line).nonEmpty).toSeq
  println(opStrings.mkString("\n"))

  import MCM.AOpSign._

  val mag = new Chainsaw.Architectures.BinarySFG
  mag.addVertex(0)
  val path = MAG.Path()
  val AOpConfigs = ListBuffer[AOpConfig](AOpConfig(0, 0, 0, ADD))

  opStrings.foreach { line =>
    println(s"rebuilding $line")
    val patternShiftAdd(sum, left, shift, right) = line

    def toInt(string: String): Int = {
      val digits = string.filter(_.isDigit)
      val abs = if (digits.nonEmpty) digits.toInt else 1
      if (string.startsWith("M")) -abs else abs
    }

    val coeffSum = toInt(sum)
    val tempLeft = toInt(left)
    val tempRight = toInt(right)
    val (coeffLeft, coeffRight, slLeft, slRight) =
      if (tempLeft.abs <= tempRight.abs) (tempLeft, tempRight, shift.toInt, 0)
      else (tempRight, tempLeft, 0, shift.toInt)

    val sign =
      if (coeffLeft >= 0 && coeffRight >= 0) ADD
      else if (!(coeffLeft >= 0) && !(coeffRight >= 0)) ADD
      else if (coeffLeft >= 0 && !(coeffRight >= 0)) if (coeffSum >= 0) SUBNEXT else SUBPREV
      else if (coeffSum >= 0) SUBPREV else SUBNEXT

    if (!path.contains(coeffSum.abs)) {
      println(coeffSum, coeffLeft, coeffRight)
      path += coeffSum.abs
      println(path)
      mag.addVertex(path.indexOf(coeffLeft.abs), path.indexOf(coeffRight.abs))
      println(AOpConfig(shift.toInt, 0, 0, sign))
      AOpConfigs += AOpConfig(slLeft, slRight, 0, sign)
    }
  }

  println(mag)
  println(path)

  val (graph, magInfos) = MAG.rebuildMAG(path, mag)
  val SAG = new HomogeneousBinarySFGBuilder(Seq(input), graph, AOpHardware, AOpConfigs)

  // post-processing, as current result is POF of the constant
  val complement = constant / AOperations.getPOF(constant)
  require(isPow2(complement.abs))
  val retPOF = SAG.implicitValue.head
  val retAbs = retPOF << log2Up(complement.abs)
  val ret = if (complement < 0) -retAbs else retAbs

  override def implicitValue = RegNext(ret)

  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 1, 1)
}

object SCM {

  def apply(input: Real, contant: Int): SCM = new SCM(input, contant)

  def main(args: Array[String]): Unit = {
    ChainsawDebug = true
    SpinalConfig().generateVhdl(new Component {
      val input = in(UIntReal((1 << 14) - 1))
      val sag = new SCM(input, 31)
      val output = out(sag.implicitValue)
    })
  }
}
