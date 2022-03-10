package Chainsaw.crypto.RSA

import spinal.core._

import scala.language.postfixOps

case class MontMultPEDataFlow(w: Int) extends Bundle {
  val SWord = UInt(w bits) // w-1 downto 1
  val YWord = UInt(w bits)
  val MWord = UInt(w bits)
}

case class MontMultPEControlFlow(w: Int) extends Bundle {
  val setXi = Bool()
}

case class MontMultPEFlow(w: Int) extends Bundle {
  val data = MontMultPEDataFlow(w)
  val control = MontMultPEControlFlow(w)
}

/**
 * @param w the word size of MontMul
 */
class MontMultPE(w: Int) extends Component {

  val io = new Bundle {
    val flowIn = in(MontMultPEFlow(w)) // inner communications(flows)
    val flowOut = out(MontMultPEFlow(w))
    val xi = in UInt (1 bits) // outer communications
  }

  // alias for readability
  val SComp = io.flowIn.data.SWord(w - 1 downto 1)
  val S0 = io.flowIn.data.SWord.lsb
  val YWord = io.flowIn.data.YWord
  val MWord = io.flowIn.data.MWord
  val setXi = io.flowIn.control.setXi

  // BLOCK REGISTERS
  // control flags
  val xiReg = RegNextWhen(io.xi, setXi)
  val xiInUse = Mux(setXi, io.xi, xiReg)
  val qiReg = RegNextWhen((YWord.lsb & xiInUse.asBool) ^ SComp.lsb, setXi)
  val qiInUse = Mux(setXi, (YWord.lsb & xiInUse.asBool) ^ SComp.lsb, qiReg)
  // data registers, O and E stands for odd and even
  val CO, CE = Reg(UInt(2 bits))
  val SO, SE = Reg(UInt(1 bits))
  val SLower = Reg(UInt(w - 1 bits))

  // BLOCK COMPUTATION
  // intermediate signals
  val xiYWord = Mux(xiInUse.asBool, YWord, U(0))
  val qiMWord = Mux(qiInUse, MWord, U(0))
  val C = Mux(setXi, U(0), Mux(S0, CO, CE))
  // w + 1 bits, the lower w - 1 bits are final
  val SumLower = (xiYWord(w - 2 downto 0) +^ C) +^ (qiMWord(w - 2 downto 0) +^ SComp)
  val HigherCommonPart = xiYWord.msb.asUInt +^ qiMWord.msb.asUInt // 2 bits
  val SumHigherOdd = (U(1, 1 bits) +^ SumLower(w downto w - 1)) +^ HigherCommonPart // 3 bits
  val SumHigherEven = SumLower(w downto w - 1) +^ HigherCommonPart
  // update registers
  SLower := SumLower(w - 2 downto 0) // w - 1 bits
  SO := SumHigherOdd.lsb.asUInt
  SE := SumHigherEven.lsb.asUInt
  CO := SumHigherOdd(2 downto 1)
  CE := SumHigherEven(2 downto 1)

  io.flowOut.data.SWord := (Mux(S0, SO, SE) @@ SLower) // output
  io.flowOut.data.YWord := RegNext(YWord)
  io.flowOut.data.MWord := RegNext(MWord)
  io.flowOut.control.setXi := RegNext(setXi, init = False)
}

object MontMultPE {
  import Chainsaw._
  def main(args: Array[String]): Unit = {
    VivadoSynth(new MontMultPE(16))
  }
}