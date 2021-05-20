package Chainsaw.FloPoCo.BlackBoxed

import Chainsaw.QFormatReal
import spinal.core.{BlackBox, SQ, in, log2Up, out, _}

import scala.reflect.runtime.{universe => ru}

class SCM(val wIn: Int, val n: Int) extends FloPoCoBlackBox[Real, Real] {

  override val operatorName = "IntConstMult"

  override def ruType = ru.typeOf[this.type] // this is a must-be

  val clk = in Bool()
  val input = in(QFormatReal(SQ(wIn - 1, 0)))
  input.setName("X")
  val output = out(QFormatReal(SQ(wIn - 1 + log2Up(n), 0)))
  output.setName("R")
  mapCurrentClockDomain(clk)
  noIoPrefix()

  invokeFloPoCo() // a must-be
}

class SCMWrapper(wIn: Int, constant: Int) extends FloPoCoBlackBoxWrapper[Real, Real] {
  override val input = in(QFormatReal(SQ(wIn - 1, 0)))
  override val output = out(QFormatReal(SQ(wIn - 1 + log2Up(constant), 0)))
  override val blackBox: BlackBox with FloPoCoBlackBox[Real, Real] = new SCM(wIn, constant)
  connect() // A must-be
}

object SCMWrapper {
  def main(args: Array[String]): Unit = {
    val report = SpinalConfig().generateSystemVerilog(new SCMWrapper(15, 31))
  }
}
