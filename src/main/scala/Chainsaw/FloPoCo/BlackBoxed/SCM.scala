package Chainsaw.FloPoCo.BlackBoxed

import Chainsaw.FloPoCo._
import Chainsaw.FloPoCo.{defaultOutputDir, defaultOutputPath, flopocoPath}
import Chainsaw.QFormatReal
import spinal.core.{BlackBox, Bundle, Component, SQ, in, log2Up, out}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

import scala.io.Source
import scala.sys.process.Process

class SCM(val wIn: Int, val n: Int) extends FloPoCoBlackBox[Real, Real] {

  val fileds = this.getClass.getDeclaredFields.map(_.getName)
  val argNumber = this.getClass.getConstructors.head.getParameters.length
  val argNames = fileds.take(argNumber)
  val argValues = argNames.map(getArgValueString)

  println(argNames.mkString(" "))

  case class Purchase(name: String, orderNumber: Int, var shipped: Boolean)

  val p = Purchase("Jeff Lebowski", 23819, false)

  import scala.reflect.runtime.{universe => ru}

  def getArgValueString(argName: String) = {
    val m = ru.runtimeMirror(this.getClass.getClassLoader)
    val im = m.reflect(this)
    val termSymb = ru.typeOf[SCM].decl(ru.TermName("wIn")).asTerm
    val termMirror = im.reflectField(termSymb)
    termMirror.get.toString
  }

  // name & path settings
  val operatorName = "IntConstMult"
  val blackBoxName = (operatorName +: argNames).reduce(_ + "_" + _) + "_F400_uid2"
  println(blackBoxName)
  setBlackBoxName(blackBoxName)
  addRTLPath(rtlPath)
  // interface
  val clk = in Bool()
  override val input = in(QFormatReal(SQ(wIn, 0)))
  override val output = out(QFormatReal(SQ(wIn + log2Up(n), 0)))
  // clock mapping
  mapCurrentClockDomain(clk)
  // prefix
  noIoPrefix()
  // RTL generation
  val operatorCommand = operatorName + " " +  argNames.zip(argValues).map { case (name, value) => name + "=" + value }.mkString(" ")
  println(operatorCommand)
  Process(s"./flopoco outputFile=$rtlPath $operatorCommand", new java.io.File(flopocoPath)) !
}

class SCMWrapper(wIn: Int, constant: Int) extends FloPoCoBlackBoxWrapper[Real, Real] {
  override val input = in(QFormatReal(SQ(wIn, 0)))
  override val output = out(QFormatReal(SQ(wIn + log2Up(constant), 0)))
  override val blackBox: BlackBox with FloPoCoBlackBox[Real, Real] = new SCM(wIn, constant)
  connect() // A must-be
}

object SCMWrapper {
  def main(args: Array[String]): Unit = {
    val report = SpinalConfig().generateSystemVerilog(new SCMWrapper(15, 15))
    println(report.rtlSourcesPaths.mkString("\n"))
  }
}
