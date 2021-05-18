package Chainsaw.FloPoCo.BlackBoxed

import Chainsaw.FloPoCo.{flopocoPath, _}
import Chainsaw.QFormatReal
import spinal.core.{BlackBox, SQ, in, log2Up, out, _}
import scala.reflect.runtime.{universe => ru}

import scala.sys.process.Process

class SCM(val wIn: Int, val n: Int) extends FloPoCoBlackBox[Real, Real] {

  override val operatorName = "IntConstMult"

  override def ruType = ru.typeOf[this.type] // this is a must-be

  val clk = in Bool()
  val input = in(QFormatReal(SQ(wIn, 0)))
  val output = out(QFormatReal(SQ(wIn + log2Up(n), 0)))
  mapCurrentClockDomain(clk)
  noIoPrefix()

  invokeFloPoCo() // a must-be
}

class FPConstMult(val wE_in: Int, val wF_in: Int, val wE_out: Int, val wF_out: Int, val constant: String, val cst_width: Int) extends FloPoCoBlackBox[Real, Real] {

  override val operatorName = "FPConstMult"

  override def ruType = ru.typeOf[this.type] // this is a must-be

  val clk = in Bool()
  val input = in(QFormatReal(SQ(10, 0)))
  val output = out(QFormatReal(SQ(10, 0)))
  println(operatorCommand)
  mapCurrentClockDomain(clk)
  noIoPrefix()

  invokeFloPoCo() // a must-be
}

class SCMWrapper(wIn: Int, constant: Int) extends FloPoCoBlackBoxWrapper[Real, Real] {
  override val input = in(QFormatReal(SQ(wIn, 0)))
  override val output = out(QFormatReal(SQ(wIn + log2Up(constant), 0)))
  override val blackBox: BlackBox with FloPoCoBlackBox[Real, Real] = new SCM(wIn, constant)
  connect() // A must-be
}

class SomethingElseWrapper(a: Int, b: Int, c: Int) extends FloPoCoBlackBoxWrapper[Real, Real] {
  val input = in(QFormatReal(SQ(a, 0)))
  val output = out(QFormatReal(SQ(b + log2Up(c), 0)))
  override val blackBox: BlackBox with FloPoCoBlackBox[Real, Real] = new FPConstMult(1, 2, 3, 4, "1", 5)
  connect() // A must-be
}

object SCMWrapper {
  def main(args: Array[String]): Unit = {
    //    val report = SpinalConfig().generateSystemVerilog(new SCMWrapper(15, 17))
    //    println(report.rtlSourcesPaths.mkString("\n"))
    //    println(report.getRtlString())
    //        new SomethingElse(3,4,5)
    //        new SomethingElseWrapper(3,4,5)
    val report1 = SpinalConfig().generateSystemVerilog(new SomethingElseWrapper(3, 4, 5))
  }
}
