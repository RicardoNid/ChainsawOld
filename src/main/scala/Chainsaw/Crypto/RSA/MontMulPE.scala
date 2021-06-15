package Chainsaw.Crypto.RSA

import spinal.core.{Bool, Bundle, Component, False, Mux, RegInit, RegNext, U, UInt, in, out, when}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

// these data will be passed to the successor
case class MontMulPEPass(w: Int) extends Bundle {
  // SComp ## S0 combines a SWord
  val SetXi = Bool()
  val valid = Bool()
  val S0 = UInt(1 bits) // 0
  val SComp = UInt(w - 1 bits) // w-1 downto 1
  val YWord = UInt(w bits)
  val MWord = UInt(w bits)
}

// these data are from the external logic
case class MontMulPEOther() extends Bundle {
  // xi should be externally registered
  // short as it is, it is a UInt as it has numeric semantic
  val xi = UInt(1 bits)
}

/**
 * @param w the word size of MontMul
 */
class MontMulPE(w: Int) extends Component { // we want it to be synthesized independently

  val io = new Bundle {
    val dataIn = in(MontMulPEPass(w))
    val dataOut = out(MontMulPEPass(w))
    val controlIn = in(MontMulPEOther())
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
  when(io.dataIn.SetXi) { // first cycle of e cycles
    xiInUse := io.controlIn.xi
    xi := io.controlIn.xi
    qiInUse := ((io.dataIn.YWord.lsb & xiInUse.asBool) ^ io.dataIn.SComp.lsb)
    qi := ((io.dataIn.YWord.lsb & xiInUse.asBool) ^ io.dataIn.SComp.lsb) // not S0, S1
  }.otherwise {
    xiInUse := xi
    qiInUse := qi
  }

  // intermediate signals
  val xiYWord = Mux(xiInUse.asBool, io.dataIn.YWord, U(0))
  val qiMWord = Mux(qiInUse, io.dataIn.MWord, U(0))
  val C = UInt(2 bits)
  when(io.dataIn.SetXi)(C := U(0))
    .otherwise(C := Mux(io.dataIn.S0.asBool, CO, CE))

  // w + 1 bits, the lower w - 1 bits are final
  val SumLower = (xiYWord(w - 2 downto 0) +^ C) +^
    (qiMWord(w - 2 downto 0) +^ io.dataIn.SComp)

  val HigherCommonPart = xiYWord.msb.asUInt +^ qiMWord.msb.asUInt // 2 bits
  val SumHigherOdd = (U(1, 1 bits) +^ SumLower(w downto w - 1)) +^ HigherCommonPart // 3 bits
  val SumHigherEven = SumLower(w downto w - 1) +^ HigherCommonPart
  // TODO: for sim
  val SWordRet = io.dataOut.SComp ## io.dataOut.S0
  SWordRet.simPublic()

  // TODO: should the computation be controlled by run? would that be more energy-efficient?
  // update
  SLower := SumLower(w - 2 downto 0) // w - 1 bits
  SO := SumHigherOdd.lsb.asUInt
  SE := SumHigherEven.lsb.asUInt
  CO := SumHigherOdd(2 downto 1)
  CE := SumHigherEven(2 downto 1)

  // output
  io.dataOut.S0 := SLower.lsb.asUInt
  io.dataOut.SComp := (Mux(io.dataIn.S0.asBool, SO, SE) ## SLower(w - 2 downto 1)).asUInt
  io.dataOut.YWord := RegNext(io.dataIn.YWord)
  io.dataOut.MWord := RegNext(io.dataIn.MWord)
  io.dataOut.SetXi := RegNext(io.dataIn.SetXi)
  io.dataOut.SetXi.init(False)
  io.dataOut.valid := RegNext(io.dataIn.valid)
  io.dataOut.valid.init(False)
}