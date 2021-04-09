package DSP

import DSP.FIR.{DAFIR, systolicFIR}
import breeze.numerics.{abs, ceil}
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import java.lang.Math.max

class FIR(input: SFix, coefficients: IndexedSeq[Double], firArch: FIRArch) extends ImplicitArea[SFix] with DSPDesign {

  val multBitWidthGrowth = log2Up(ceil(coefficients.map(abs(_)).max).toInt) + 1
  val accBitWidthGrowth = log2Up(coefficients.length)
  val bitWidthGrowth = multBitWidthGrowth + accBitWidthGrowth

  val N = coefficients.length
  val B = input.bitCount
  val result = SFix(input.maxExp + bitWidthGrowth exp, input.minExp exp)
  var delay = 0

  firArch match {
    case FIRArch.MAC => {
      result := systolicFIR(input, coefficients)
      delay = 2 * N - (N - 1) / 2
    }
    case FIRArch.RAG =>
    case FIRArch.DA => {
      //  TODO: avoid the bitGrowth in the fractional part through ShiftAdderTree
      result := DAFIR(input, coefficients).truncated
      delay = N / 2 + 2
    }
  }

  override def implicitValue: SFix = result

  override def getDelay: Int = delay
}

object FIR {

  // y = \Sigma_{i=0}^{n-1}x[i]c[n-1-i]
  // accurate bitWidth may not be important as this is for dsp slices
  def systolicFIR(input: SFix, coefficients: IndexedSeq[Double]) = {

    // types
    def typeX = SFix(peak = input.maxExp exp, input.minExp exp)

    val multBitGrowth = log2Up(ceil(coefficients.map(abs(_)).max).toInt)

    def typeMult(i: Int) = SFix(input.maxExp + multBitGrowth exp, input.minExp exp)

    val accBitGrowth = multBitGrowth + log2Up(coefficients.length)

    def typeAcc = SFix(input.maxExp + accBitGrowth exp, input.minExp exp)

    val coeffWires = coefficients.map { coeff =>
      val maxExp = log2Up(ceil(abs(coeff) + 1).toInt)
      //  TODO: implement parametric coefficient precision
      SF(coeff, maxExp exp, input.minExp exp)
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
    //  NUMERIC: drop the bitGrowth in the fractional part on multiplication
    multRegs(0) := (input * coeffWires(0)).truncated
    (1 until n).foreach(i => multRegs(i) := (xRegs1(i - 1) * coeffWires(i)).truncated)
    accRegs(0) := multRegs(0)
    (1 until n).foreach(i => accRegs(i) := (accRegs(i - 1) + multRegs(i)))

    accRegs.last
  }

  def DAFIR(input: SFix, coefficients: IndexedSeq[Double]) = {

    // the arch below is fully-pipelined
    val n = coefficients.length
    val b = input.bitCount

    val srl: Vec[SFix] = History(input, n)
    val LUTIns = (0 until b).map(i => srl.map(_.raw(i))).map(B(_))
    printlnWhenDebug(s"input number: ${LUTIns.length}, input bitWidth: ${LUTIns(0).getBitsWidth}")

    val LUTOuts = LUTIns.map(DALUT(_, coefficients, input.minExp))
    // the real shifts should be minExp until maxExp
    val shifts = (0 until b - 1).map(_ + input.minExp)
    val init = ShiftAdderTree(LUTOuts.init, shifts).implicitValue
    LUTIns(b - 1).setName("signBit").simPublic()
    LUTOuts(b - 1).setName("signBitResult").simPublic()
    val last = RegNext(LUTOuts.last << (b - 1 + input.minExp))
    init - last
  }

  def DALUT(input: Bits, coefficients: IndexedSeq[Double], LUTMinExp: Int) = {
    val length = input.getBitsWidth
    val tableContents = (0 until 1 << length).map { i =>
      val bits = i.toBinaryString
      val paddedBits = "0" * (length - bits.length) + bits
      val ones = coefficients.reverse.zip(paddedBits).filter { case (coeff, bit) => bit == '1' }.map { case (coeff, bit) => coeff }
      if (ones.isEmpty) 0.0 else ones.reduce(_ + _)
    }
    //    printlnWhenDebug(s"DALUT content: ${tableContents.mkString(" ")}")
    //  range of sum of coefficients
    val LUTContentMax = max(coefficients.filter(_ > 0.0).sum, abs(coefficients.filter(_ < 0.0).sum))
    val LUTMaxExp = log2Up(ceil(LUTContentMax).toInt + 1)
    //    println(s"LUT $LUTContentMax, $LUTMaxExp")
    //  TODO: implement parametric coefficient precision
    val fixedTableContents = tableContents.map(coeff => SF(coeff, LUTMaxExp exp, LUTMinExp exp))

    val LUT = Mem(fixedTableContents)
    LUT.readSync(input.asUInt)
  }

  def apply(input: SFix, coefficients: IndexedSeq[Double], firArch: FIRArch): FIR = new FIR(input, coefficients, firArch)
}
