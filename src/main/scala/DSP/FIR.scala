package DSP

import DSP.FIR.systolicFIR
import breeze.numerics.{abs, ceil}
import spinal.core._
import spinal.lib._


sealed trait FIRArch

object FIRArch {

  case object MAC extends FIRArch

  case object RAG extends FIRArch

  case object DA extends FIRArch

}

import DSP.FIRArch._

class FIR(coefficients: IndexedSeq[Double],
          FIRArch: FIRArch
         ) extends Component {

  val bitWidthGrowth = log2Up(coefficients.length + 1) + log2Up(ceil(coefficients.map(abs(_)).max + 1).toInt)

  val io = new Bundle {
    val input = slave Flow data
    val output = master Flow SFix(peak = (data.maxExp + bitWidthGrowth) exp, resolution = data.minExp exp)
  }

  FIRArch match {
    case MAC => {
      val ZERO = SF(0.0, data.maxExp exp, data.bitCount bits)
      val actualInput = Mux(io.input.valid, io.input.payload, ZERO)
      // TODO: think about reverse carefully
      io.output.payload := systolicFIR(actualInput, coefficients)
      val n = coefficients.length
      io.output.valid := Delay(io.input.valid, 2 * n - (n - 1) / 2, init = False)
      io.output.valid.init(False)
    }
    case RAG =>
    case DA =>
  }
}

object FIR {

  // y = \Sigma_{i=0}^{n-1}x[i]c[n-1-i]
  // accurate bitWidth may not be important as this is for dsp slices
  def systolicFIR(input: SFix, coefficients: IndexedSeq[Double]) = {

    // types
    def typeX = SFix(peak = input.maxExp exp, width = input.bitCount bits)

    val multBitGrowth = log2Up(ceil(coefficients.map(abs(_)).max + 1).toInt)

    def typeMult(i: Int) = SFix(peak = (input.maxExp + multBitGrowth) exp, width = (input.bitCount + multBitGrowth) bits)

    val accBitGrowth = multBitGrowth + log2Up(coefficients.length + 1)

    def typeAcc = SFix(peak = input.maxExp + accBitGrowth exp, width = input.bitCount + accBitGrowth bits)

    val coeffWires = coefficients.map { coeff =>
      val maxExp = log2Up(ceil(abs(coeff) + 1).toInt)
      //      val maxExp = 4
      val bitCount = input.bitCount - input.maxExp + maxExp
      SF(coeff, peak = maxExp exp, width = bitCount bits)
    }

    //  regs
    val n = coefficients.length
    val xRegs0 = (0 until n - 1).map(i => Reg(typeX))
    val xRegs1 = (0 until n - 1).map(i => Reg(typeX))
    val multRegs = (0 until n).map(i => Reg(typeMult(i)))
    val accRegs = (0 until n).map(i => Reg(typeAcc)) // bitWidth should be inferred

    //  connections
    xRegs0(0) := input
    (1 until n - 1).foreach(i => xRegs0(i) := xRegs1(i - 1))
    (0 until n - 1).foreach(i => xRegs1(i) := xRegs0(i))
    multRegs(0) := (input * coeffWires(0)).truncated
    (1 until n).foreach(i => multRegs(i) := (xRegs1(i - 1) * coeffWires(i)).truncated)
    accRegs(0) := multRegs(0).truncated
    (1 until n).foreach(i => accRegs(i) := (accRegs(i - 1) + multRegs(i)).truncated)

    accRegs.last
  }

  def DAFIR(input: SFix, coefficients: IndexedSeq[Double]) = {

    // the arch below is fully-pipelined
    def typeX = SFix(peak = input.maxExp exp, width = input.bitCount bits)

    val n = coefficients.length
    val b = input.bitCount
    val srl = History(input, n)
    val LUTIns = (0 until b).map(i => srl.map(_.raw(i))).map(B(_))
    val LUTOuts = LUTIns.map(DALUT(_, coefficients, data.maxExp, data.bitCount))
    val shiftedLUTOuts = (0 until b).map(i => LUTOuts(i) << i)
    //  TODO: implement a well-tested and widely adaptive shift-add tree / graph before this
    val result = AdderTree(shiftedLUTOuts.map(_.raw)).implicitValue

  }

  def DALUT(input: Bits, coefficients: IndexedSeq[Double], coeffMaxExp: Int, coeffBitCount: Int) = {
    val length = input.getBitsWidth
    val tableContents = (0 until 1 << length).map { i =>
      val bits = i.toBinaryString
      coefficients.zip(bits).filter { case (coeff, bit) => bit == '1' }.map { case (coeff, bit) => coeff }.reduce(_ + _)
    }
    val fixedTableContents = tableContents.map(coeff => SF(coeff, coeffMaxExp exp, coeffBitCount bits))

    val LUT = Mem(SFix(coeffMaxExp exp, coeffBitCount bits), fixedTableContents)
    LUT.readSync(input.asUInt)
  }

  def main(args: Array[String]): Unit = {

  }
}
