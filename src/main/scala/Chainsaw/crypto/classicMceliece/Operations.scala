package Chainsaw.crypto.classicMceliece

import Chainsaw.crypto.classicMceliece.gfOperators
import Chainsaw.dspTest.{DSPTestable, doFlowPeekPokeTest, setMonitor}
import algebra.lattice.Bool
import spinal.core
import spinal.core.{Bits, Component, False, IntToBuilder, LongToBits, Mux, RegNext, Vec, report, when}
import spinal.lib.{Delay, Flow, master, memPimped, slave}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import Chainsaw._

/** @see
  *   [[https://dl.acm.org/doi/10.1145/1723112.1723122]] we take the names of variables from this paper
  */
object Operations {

  def bitrev(a: Bits): Bits = {
    //reverse the input bits and resize
    a.reversed.resize(GfBits bits)
  }

  def eval(dataIn: Vec[Bits]): Bits = {
    require(dataIn.length == CorrectionNum + 2)

    val coeffs = dataIn.init // length = CorrectionNum + 1
    val data   = dataIn.last
    val temp1  = Seq.fill(CorrectionNum)(Bits(GfBits bits))
    val temp2  = Seq.fill(CorrectionNum)(Bits(GfBits bits))

    (coeffs.last +: temp2.init).zip(temp1).zip(temp2).zipWithIndex.foreach { case (((line1, line2), line3), i) =>
      line2 := gfOperators.gfMul(line1, data)
      line3 := gfOperators.gfAdd(line2, coeffs(CorrectionNum - 1 - i))
    }
    temp2.last

  }

  def root(dataIn: Vec[Bits]): Vec[Bits] = {
    require(dataIn.length == CorrectionNum + 1 + CodeLength)
    //the first CorrectionNum+1 elements of dataIn are the coeffs of Polynominal,also called f
    val PolyF: IndexedSeq[Bits] = dataIn.take(CorrectionNum + 1)
    //the last Codelength elements of dataIn are the Vector of L
    val ListElement = dataIn.takeRight(CodeLength)
    val Result      = Vec(Bits(GfBits bits), CodeLength)
    ListElement.zip(Result).foreach { case (in, out) =>
      out := eval(Vec(PolyF :+ in)) // change IndexSeq to Vec to match the eval 's interface
    }
    Result

  }

  def SameMask(dataIn: Vec[Bits]): Bits = {
    //function: if tow input data are same ,return 8 bit "1"
    require(dataIn.length == 2)
    val compare = dataIn(0) ^ dataIn(1)
    val value   = compare.xorR // if same,value=0
    val result  = Bits(8 bits)
    result.setAllTo(~value)
    result

  }

  def syndrome(dataIn: Vec[Bits]): Bits = {
    require(dataIn.length == GfBits * CorrectionNum + 1)
    //todo:divide into two inputs,because the actual bit length of the Vec now is different
    val eVector = dataIn.last.asBits //the last one of dataIn is the error vector,has CodeLength bits
    //the others of dataIn is GfBits*CorrectionNum rows of pk,each row has (CodeLength-GfBits*CorrectionNum) bits
    val syndResult = Bits(GfBits * CorrectionNum bits)
    (0 until (GfBits * CorrectionNum)).foreach { i =>
      syndResult(i) := ((dataIn(i) & eVector).xorR) ^ eVector(CodeLength - 1 - i)
    }
    syndResult

  }

  case class TestGFMUL() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {

    override val dataIn: Flow[Vec[Bits]]  = slave Flow Vec(Bits(12 bits), CorrectionNum + 1 + CodeLength)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), CodeLength)
    override val latency: Int             = 1
    //val dataIn1: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 1)

    dataOut.payload := RegNext(Vec(Operations.root(dataIn.payload)))
    dataOut.valid   := Delay(dataIn.valid, latency, init = False)
  }

  case class TestSameMask() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {

    override val dataIn: Flow[Vec[Bits]]  = slave Flow Vec(Bits(12 bits), 2)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 1)
    override val latency: Int             = 1
    //val dataIn1: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 1)

    dataOut.payload := RegNext(Vec(Operations.SameMask(dataIn.payload)))
    dataOut.valid   := Delay(dataIn.valid, latency, init = False)
  }

  case class TestSyndrome() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {

    override val dataIn: Flow[Vec[Bits]]  = slave Flow Vec(Bits(CodeLength bits), GfBits * CorrectionNum + 1)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(GfBits * CorrectionNum bits), 1)
    //override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(CodeLength bits), GfBits*CorrectionNum+2)
    override val latency: Int = 1

    dataOut.payload := RegNext(Vec(Operations.syndrome(dataIn.payload)))
    dataOut.valid   := Delay(dataIn.valid, latency, init = False)
  }

}

object TestOperations {
  def main(args: Array[String]): Unit = {

//    val seq1 = Seq.fill(CorrectionNum+1)(0).zipWithIndex.map {
//      case (i, kk) => BigInt(kk+5)
//    }
//    val testCase = seq1 :+ (BigInt("2", 10)):+ (BigInt("3", 10)):+ (BigInt("4", 10))
//
//    val GoldenCase = Seq("665","ccd","481").map(BigInt(_, 16))

    val testCase   = Seq("92ff27", "9865", "0259", "3457", "ff27", "9865", "0259", "3457", "5920592").map(BigInt(_, 16))
    val GoldenCase = Seq("7c").map(BigInt(_, 16))

    doFlowPeekPokeTest("TestGf", Operations.TestSyndrome(), Seq(testCase), GoldenCase)
  }

}
