package Chainsaw.FloPoCo.Transplanted

import Chainsaw.Architectures.HomogeneousBinarySFGBuilder
import Chainsaw.FloPoCo.BlackBoxed.SCMWrapper
import Chainsaw.FloPoCo.flopocoPath
import Chainsaw.MCM.AOperations.AOpHardware
import Chainsaw.MCM.MAG
import Chainsaw._
import spinal.core._
import xilinx.VivadoFlow

import scala.io.Source
import scala.sys.process.Process

class SCM(input: Real, constant: Int) extends ImplicitArea[Real] with Testable {
  // invoke flopoco and get source
  val command = s"IntConstMult wIn=${input.bitCount} n=$constant"
  Process(s"./flopoco $command", new java.io.File(flopocoPath)) !
  val rtl = Source.fromFile(flopocoPath + "/flopoco.vhdl")

  val patternShiftAdd = "-- [P|M]".r
  // extract the design from the source
  val opStrings = rtl.getLines().toSeq.filter(patternShiftAdd.findFirstIn(_).nonEmpty).drop(1)
  println(opStrings.mkString("\n"))
  val patternVariable = "[P|M]?[0-9]*X".r
  val patternDigit = "[0-9]+".r

  def toInt(string: String) = {
    if (string.forall(!_.isDigit)) 1
    else (string.filter(_.isDigit).toInt)
  }

  val operands: Seq[Seq[Int]] =
    opStrings.map(patternVariable.findAllIn(_).toSeq // get patterns like P101X, X
      .map(patternDigit.findFirstIn(_).getOrElse("1").toInt)) // P101X -> 101, X -> 1

  println(operands.map(_.mkString(" ")).mkString("\n"))

  // rebuild the design by Chainsaw
  val mag = new Chainsaw.Architectures.BinarySFG
  mag.addVertex(0)
  val path = MAG.Path()
  operands.foreach { number =>
    path += number(0)
    println(path)
    mag.addVertex(path.indexOf(number(1)), path.indexOf(number(2)))
  }
  println(mag)
  println(path)

  val (graph, magInfos) = MAG.rebuildMAG(path, mag)
  val SAG = new HomogeneousBinarySFGBuilder(Seq(input), graph, AOpHardware, magInfos)

  override def implicitValue = RegNext(SAG.implicitValue.head)

  override val getTimingInfo: TimingInfo = TimingInfo(1,1,1,1)
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
