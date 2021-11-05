package Chainsaw.tobeTransplanted

import spinal.core.{Real, SQ, in, out}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

import scala.reflect.runtime.{universe => ru}

class FPConstMult(val wE_in: Int, val wF_in: Int, val wE_out: Int, val wF_out: Int, val constant: String, val cst_width: Int) extends FloPoCoBlackBox[Real, Real] {

  override val operatorName = "FPConstMult"

  override def ruType = ru.typeOf[this.type] // this is a must-be

  val clk = in Bool()
  val input = in(QFormatReal(SQ(10, 0)))
  val output = out(QFormatReal(SQ(10, 0)))
  mapCurrentClockDomain(clk)
  noIoPrefix()

  invokeFloPoCo() // a must-be
}


