package Chainsaw.Crypto.RSA

import spinal.core.{Bool, Bundle, Component, False, Mux, RegInit, RegNext, U, UInt, in, out, when, _}
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

case class MontMulPEDataFlow(w: Int) extends Bundle {
  val SWord = UInt(w - 1 bits) // w-1 downto 1
  val YWord = UInt(w bits)
  val MWord = UInt(w bits)
}

case class MontMulPEControlFlow(w: Int) extends Bundle{
  val SetXi = Bool()
  val valid = Bool()
}

// these data will be passed to the successor
case class MontMulPEFlow(w: Int) extends Bundle {
  // SComp ## S0 combines a SWord
//  val data = MontMulPEDataFlow(w)
//  val control = MontMulPEControlFlow
  val SetXi = Bool()
  val valid = Bool()
  val S0 = UInt(1 bits) // 0
  val SComp = UInt(w - 1 bits) // w-1 downto 1
  val YWord = UInt(w bits)
  val MWord = UInt(w bits)
}

/**
 * @param w the word size of MontMul
 */
class MontMulPE(w: Int) extends Component { // we want it to be synthesized independently

  val io = new Bundle {
    val flowIn = in(MontMulPEFlow(w))
    val flowOut = out(MontMulPEFlow(w))
    val xi = in UInt(1 bits)
  }

  // data registers
  val CO, CE = RegInit(U(0, 2 bits))
  val SO, SE = RegInit(U(0, 1 bits))
  val SLower = RegInit(U(0, w - 1 bits))

  // long-term data registers
  val qi = RegInit(False)
  val qiInUse = Bool()
  val xi = RegInit(U(0, 1 bits))
  val xiInUse = UInt(1 bits)
  when(io.flowIn.SetXi) { // first cycle of e cycles
    xiInUse := io.xi
    xi := io.xi
    qiInUse := ((io.flowIn.YWord.lsb & xiInUse.asBool) ^ io.flowIn.SComp.lsb)
    qi := ((io.flowIn.YWord.lsb & xiInUse.asBool) ^ io.flowIn.SComp.lsb) // not S0, S1
  }.otherwise {
    xiInUse := xi
    qiInUse := qi
  }

  // intermediate signals
  val xiYWord = Mux(xiInUse.asBool, io.flowIn.YWord, U(0))
  val qiMWord = Mux(qiInUse, io.flowIn.MWord, U(0))
  val C = UInt(2 bits)
  when(io.flowIn.SetXi)(C := U(0))
    .otherwise(C := Mux(io.flowIn.S0.asBool, CO, CE))

  // w + 1 bits, the lower w - 1 bits are final
  val SumLower = (xiYWord(w - 2 downto 0) +^ C) +^
    (qiMWord(w - 2 downto 0) +^ io.flowIn.SComp)

  val HigherCommonPart = xiYWord.msb.asUInt +^ qiMWord.msb.asUInt // 2 bits
  val SumHigherOdd = (U(1, 1 bits) +^ SumLower(w downto w - 1)) +^ HigherCommonPart // 3 bits
  val SumHigherEven = SumLower(w downto w - 1) +^ HigherCommonPart
  // TODO: for sim
  val SWordRet = io.flowOut.SComp ## io.flowOut.S0
  SWordRet.simPublic()

  // TODO: should the computation be controlled by run? would that be more energy-efficient?
  // update
  SLower := SumLower(w - 2 downto 0) // w - 1 bits
  SO := SumHigherOdd.lsb.asUInt
  SE := SumHigherEven.lsb.asUInt
  CO := SumHigherOdd(2 downto 1)
  CE := SumHigherEven(2 downto 1)

  // output
  io.flowOut.S0 := SLower.lsb.asUInt
  io.flowOut.SComp := (Mux(io.flowIn.S0.asBool, SO, SE) ## SLower(w - 2 downto 1)).asUInt
  io.flowOut.YWord := RegNext(io.flowIn.YWord)
  io.flowOut.MWord := RegNext(io.flowIn.MWord)
  io.flowOut.SetXi := RegNext(io.flowIn.SetXi)
  io.flowOut.SetXi.init(False)
  io.flowOut.valid := RegNext(io.flowIn.valid)
  io.flowOut.valid.init(False)
}