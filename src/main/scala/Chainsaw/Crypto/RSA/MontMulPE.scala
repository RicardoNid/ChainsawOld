package Chainsaw.Crypto.RSA

import spinal.core.{Bool, Bundle, Component, False, Mux, RegInit, RegNext, U, UInt, in, out, when, _}
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

case class MontMulPEDataFlow(w: Int) extends Bundle {
  val SWord = UInt(w bits) // w-1 downto 1
  val YWord = UInt(w bits)
  val MWord = UInt(w bits)
}

case class MontMulPEControlFlow(w: Int) extends Bundle {
  val SetXi = Bool()
  val valid = Bool()
}

case class MontMulPEFlow(w: Int) extends Bundle {
  val data = MontMulPEDataFlow(w)
  val control = MontMulPEControlFlow(w)
}

/**
 * @param w the word size of MontMul
 */
class MontMulPE(w: Int) extends Component { // we want it to be synthesized independently

  val io = new Bundle {
    // inner communications(flows)
    val flowIn = in(MontMulPEFlow(w))
    val flowOut = out(MontMulPEFlow(w))
    // outer communications
    val xi = in UInt (1 bits)
  }

  // alias for readability
  val SComp = io.flowIn.data.SWord(w - 1 downto 1)
  val S0 = io.flowIn.data.SWord.lsb
  val YWord = io.flowIn.data.YWord
  val MWord = io.flowIn.data.MWord
  val SetXi = io.flowIn.control.SetXi
  val valid = io.flowIn.control.valid

  // data registers
  val CO, CE = RegInit(U(0, 2 bits))
  val SO, SE = RegInit(U(0, 1 bits))
  val SLower = RegInit(U(0, w - 1 bits))

  // long-term data registers
  val qi = RegInit(False)
  val qiInUse = Bool()
  val xi = RegInit(U(0, 1 bits))
  val xiInUse = UInt(1 bits)
  when(SetXi) { // first cycle of e cycles
    xiInUse := io.xi
    xi := io.xi
    qiInUse := ((YWord.lsb & xiInUse.asBool) ^ SComp.lsb)
    qi := ((YWord.lsb & xiInUse.asBool) ^ SComp.lsb) // not S0, S1
  }.otherwise {
    xiInUse := xi
    qiInUse := qi
  }

  // intermediate signals
  val xiYWord = Mux(xiInUse.asBool, YWord, U(0))
  val qiMWord = Mux(qiInUse, MWord, U(0))
  val C = UInt(2 bits)
  when(SetXi)(C := U(0))
    .otherwise(C := Mux(S0, CO, CE))

  // w + 1 bits, the lower w - 1 bits are final
  val SumLower = (xiYWord(w - 2 downto 0) +^ C) +^
    (qiMWord(w - 2 downto 0) +^ SComp)

  val HigherCommonPart = xiYWord.msb.asUInt +^ qiMWord.msb.asUInt // 2 bits
  val SumHigherOdd = (U(1, 1 bits) +^ SumLower(w downto w - 1)) +^ HigherCommonPart // 3 bits
  val SumHigherEven = SumLower(w downto w - 1) +^ HigherCommonPart
  // TODO: for sim
  val SWordRet = io.flowOut.data.SWord
  SWordRet.simPublic()

  // update
  SLower := SumLower(w - 2 downto 0) // w - 1 bits
  SO := SumHigherOdd.lsb.asUInt
  SE := SumHigherEven.lsb.asUInt
  CO := SumHigherOdd(2 downto 1)
  CE := SumHigherEven(2 downto 1)

  // output
  io.flowOut.data.SWord := (Mux(S0, SO, SE) @@ SLower)
  io.flowOut.data.YWord := RegNext(YWord)
  io.flowOut.data.MWord := RegNext(MWord)

  io.flowOut.control.SetXi := RegNext(SetXi)
  io.flowOut.control.SetXi.init(False)
  io.flowOut.control.valid := RegNext(valid)
  io.flowOut.control.valid.init(False)
}