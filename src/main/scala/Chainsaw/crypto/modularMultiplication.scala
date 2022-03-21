package Chainsaw.crypto

import spinal.core._

object modularMultiplication {

  case class PEIO(w: Int) extends Bundle {
    val SLsb  = Bool
    val SHigh = UInt(w - 1 bits)
    val YWord = UInt(w bits)
    val MWord = UInt(w bits)
    val start = Bool()
  }

  case class PE(w: Int) extends Component {
    val pipeIn  = in(PEIO(w))
    val pipeOut = out(PEIO(w))
    val xi      = in Bool ()

    import pipeIn._

    def zero = U(0, w bits)

    val qi      = (xi & YWord.lsb) ^ SHigh.lsb
    val qiReg   = RegNextWhen(qi, start)
    val qiInUse = Mux(start, qi, qiReg)

    val sEven = (B"0" ## SHigh).asUInt
    val sOdd  = (B"1" ## SHigh).asUInt

    val evenReg, oddReg = Reg(UInt(w + 2 bits))
    val trueRet         = Mux(SLsb, oddReg, evenReg)
    val (cOut, sOut)    = trueRet.splitAt(w)
    val cInUse          = Mux(start, U(0, 2 bits), cOut.asUInt)

    def core(xi: Bool, qi: Bool, Y: UInt, M: UInt, S: UInt, C: UInt) =
      (Mux(xi, Y, zero) +^ Mux(qi, M, zero)) +^ (S +^ C)

    evenReg := core(xi, qiInUse, YWord, MWord, sEven, cInUse) // w + 2 bits
    oddReg  := core(xi, qiInUse, YWord, MWord, sOdd, cInUse)

    pipeOut.start := RegNext(start, init = False)
    pipeOut.MWord := RegNext(MWord)
    pipeOut.YWord := RegNext(YWord)
    pipeOut.SHigh := sOut(w - 1 downto 1).asUInt
    pipeOut.SLsb  := sOut.lsb
  }

}
