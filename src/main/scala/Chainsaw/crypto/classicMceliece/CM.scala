package Chainsaw.crypto.classicMceliece

import Chainsaw.crypto.symmetric.SM4Config
import Chainsaw.dspTest.{DSPTestable, doFlowPeekPokeTest}
import breeze.linalg.DenseVector.space_Double.hasOps
import breeze.linalg.{InjectNumericOps, rand}
import org.scalatest.flatspec.AnyFlatSpec
import spinal._
import spinal.core._
import spinal.lib.{Delay, Flow, boolPimped, master, slave}

import scala.+:
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object gfOperators {


  def gfIsZero(a: Bits): Bits = {
    //todo:the result of gfIsZero C code is 13 bits,or just need 12 bits?
    val allZero=Bits(GfBits+1 bits)
    val allOne= Bits(GfBits+1 bits)
    allZero.setAllTo(false)
    allOne.setAllTo(true)
    Mux(a.orR, allZero, allOne)
  }

  def gfAdd(a: Bits, b: Bits): Bits = {
    a ^ b
  }

  def gfMul(a: Bits, b: Bits): Bits = {
    require(a.getBitsWidth == GfBits && b.getBitsWidth == GfBits)
    val aPadded: Bits = a.resize(2 * GfBits - 1)

// stack overflow code:
//    val tem = ArrayBuffer(aPadded)
//    (0 until GfBits-1).foreach{ i =>
//      tem += Mux(b(i + 1), tem.last ^ (aPadded |<< i+1), tem.last)
//    }
    val x = Seq.fill(GfBits)(Bits(2*GfBits-1 bits))
    val select=Seq.fill(GfBits)(Bits(2*GfBits-1 bits))
    select(0).setAllTo(~b(0))
    (1 until(GfBits)).foreach(i=> select(i).setAllTo(b(i)))

    (aPadded+:x.init)
      .zip(x)
      .zipWithIndex.foreach{ case ((prev,next),i)=>
      next:=prev^((aPadded|<<i)&select(i))
    }


    //todo: add polynominal f into config
    val temp = ArrayBuffer(x.last)
    val t1 = temp.last & B"23'x7fc000"
    temp += (temp.last ^ (t1 |>> 9))
    temp += (temp.last ^ (t1 |>> 12))
    val t2 = temp.last & (B"23'x3000")
    temp += (temp.last ^ (t2 |>> 9))
    temp += (temp.last ^ (t2 |>> 12))

    temp.last.take(GfBits)
  }

  def gfSq(a: Bits): Bits = {
    //Square
    gfMul(a, a)
  }

  def gfInv(a: Bits): Bits = {
    var out = a

    //tem11=a^3
    out = gfSq(out)
    var tem11 = gfMul(out, a)

    //tem1111=a^15
    out = gfSq(tem11)
    out = gfSq(out)
    var tem1111 = gfMul(out, tem11)

    out = gfSq(tem1111)
    out = gfSq(out)
    out = gfSq(out)
    out = gfSq(out)
    out = gfMul(out, tem1111)
    //now out=a^255

    out = gfSq(out)
    out = gfSq(out)
    out = gfMul(out, tem11)
    //now out=a^1023

    out = gfSq(out)
    out = gfMul(out, a)
    //now out=a^2047

    out = gfSq(out)
    //now out=a^4094
    out
  }

  def gfFrac(a: Bits, b: Bits): Bits = {
    gfMul(gfInv(a), b)
  }

  def GFMul(dataIn: Vec[Bits]): Vec[Bits] = {
    //todo: require t>4
    ///*
    require(dataIn.length==CorrectionNum*2)
    //depart input into two parameters:
    val paraA=dataIn.take(CorrectionNum)      // the first CorrectionNum elements
    val paraB=dataIn.takeRight(CorrectionNum) // the last CorrectionNum elements


    // a matrix to save temp prod of two input Vectors
    val tmp = Vec(Vec(Bits(GfBits bits), CorrectionNum), CorrectionNum)
    for (i <- 0 until CorrectionNum) {
      (0 until (CorrectionNum)).foreach(j => tmp(i)(j) := gfMul(paraA(j), paraB(i)))
    }

    // to get the prod from the temp matrix
    val prod = Vec(Bits(12 bits), CorrectionNum * 2 - 1)
    // add the element in temp matrix to get the prod,and Addtemp save the temp results
    var Addtemp = mutable.Buffer(Bits(GfBits bits))
    for (i <- 0 until (CorrectionNum)) {
      Addtemp.append(tmp(0)(i))
      (1 until i + 1).foreach { j =>
        Addtemp.append(Addtemp.last ^ tmp(j)(i - j))
      }
      prod(i) := Addtemp.last
    }

    for (i <- CorrectionNum until (2 * CorrectionNum - 1)) {
      Addtemp.append(tmp(i - CorrectionNum + 1)(CorrectionNum - 1))
      ((i - (CorrectionNum - 1) + 1) until CorrectionNum).foreach { j =>
        Addtemp.append(Addtemp.last ^ tmp(j)(i - j))
      }
      prod(i) := Addtemp.last
    }


    val mul2 = Vec(Bits(GfBits bits), CorrectionNum + 2)
    (2 until CorrectionNum - 1).foreach(i => mul2(i) := prod(i) ^ gfMul(prod(i + CorrectionNum), (B"12'x002")))
    mul2(CorrectionNum - 1) := prod(CorrectionNum - 1)
    mul2(CorrectionNum) := prod(CorrectionNum)
    mul2(CorrectionNum + 1) := prod(CorrectionNum + 1)

    val prod1 = Vec(Bits(GfBits bits), CorrectionNum + 2)
    val prod2 = Vec(Bits(GfBits bits), CorrectionNum)
    for (i <- (CorrectionNum - 1) * 2 to 2 * CorrectionNum - 3 by -1) {
      prod1(i - CorrectionNum + 3) := prod(i) ^ mul2(i - CorrectionNum + 3)
      prod1(i - CorrectionNum + 1) := prod(i) ^ mul2(i - CorrectionNum + 1)
    }
    (0 until 2).foreach(i => mul2(i) := prod(i) ^ gfMul(prod1(i + CorrectionNum), (B"12'x002")))

    for (i <- 2 * CorrectionNum - 4 to CorrectionNum + 2 by -1) {
      prod2(i - CorrectionNum + 3) := prod(i) ^ prod1(i - CorrectionNum + 3)
      prod1(i - CorrectionNum + 1) := prod(i) ^ mul2(i - CorrectionNum + 1)
    }
    for (i <- CorrectionNum + 1 to CorrectionNum by -1) {
      prod2(i - CorrectionNum + 3) := prod1(i) ^ prod1(i - CorrectionNum + 3)
      prod1(i - CorrectionNum + 1) := prod1(i) ^ mul2(i - CorrectionNum + 1)
    }

    prod1(0) := mul2(0)
    prod2(2) := prod1(2)
    prod2(1) := prod1(1)
    prod2(0) := prod1(0)
    prod2
    //*/
    //todo:try a new method, unfinished
//    val tmp = Vec(Vec(Bits(GfBits bits), CorrectionNum), CorrectionNum)
//    for (i <- 0 until CorrectionNum) {
//      (0 until (CorrectionNum)).foreach(j => tmp(i)(j) := gfMul(a(j), a(CorrectionNum + i)))
//    }
//    //val prod1 = Seq.fill(CorrectionNum*2-1)(Bits(width = GfBits bits))
//    val prod =Seq.fill(CorrectionNum*2-1)(Bits(GfBits bits))
//    var ortmp = mutable.Buffer(Bits(GfBits bits))
//    for (i <- 0 until (CorrectionNum)) {
//      ortmp.append(tmp(0)(i))
//      (1 until i + 1).foreach { j =>
//        ortmp.append(ortmp.last ^ tmp(j)(i - j))
//      }
//      prod(i) := ortmp.last
//    }
//
//    for (i <- CorrectionNum until (2 * CorrectionNum - 1)) {
//      ortmp.append(tmp(i - CorrectionNum + 1)(CorrectionNum - 1))
//      ((i - (CorrectionNum - 1) + 1) until CorrectionNum).foreach { j =>
//        ortmp.append(ortmp.last ^ tmp(j)(i - j))
//      }
//      prod(i) := ortmp.last
//    }
//
//    //val prod = Vec(Bits(GfBits bits),CorrectionNum*2-1)
//    val prodReversed=prod.reverse
//    val prod1=Seq.fill(CorrectionNum+2)(Bits( GfBits bits))// high to low
//    val prod2=Seq.fill(CorrectionNum)(Bits(GfBits bits))// high to low
//    val prod3=Seq.fill(CorrectionNum-1)(Bits(GfBits bits))
//
//    //val prod1=Vec(Bits(GfBits bits),CorrectionNum+2)
//
//
//    (prodReversed.take(CorrectionNum-3)+:prod1.take(2))
//      .zip(prodReversed.takeRight(CorrectionNum+2).take(3)+:prod3.take(CorrectionNum-4))
//      .zip(prod1.take(CorrectionNum-1))
//      .zip(prodReversed(CorrectionNum-1)+:prod3.take(CorrectionNum-2))
//      .zip(prod2.take(CorrectionNum-1))
//      .zip(prodReversed.takeRight(CorrectionNum-1))
//      .zip(prod3).foreach{
//      case ((((((s0,s1),r1),s2),r2),s3),r3) =>
//        r1:=gfMul(s0,s1)
//        r2:=gfMul(s0,s2)
//        r3:=s3^gfMul(s0,B"12'x002")
//    }
//
//
////    prodReversed.take(CorrectionNum)
////      .zip(prodReversed.takeRight(CorrectionNum+3).take(CorrectionNum))
////      .zip(prodReversed.takeRight(CorrectionNum+3).take(CorrectionNum))
////      .zip(prodReversed.takeRight(CorrectionNum+1).takeRight(CorrectionNum))
////      .zip(prodReversed.takeRight(CorrectionNum+1).takeRight(CorrectionNum))
////      .zip(prodReversed.takeRight(CorrectionNum))
////      .zip(prodReversed.takeRight(CorrectionNum)).foreach{
////       case ((((((prevHead,prev1),prod1),prev2),prod2),prev3),prod3) =>
////        prod1:=prevHead^prev1
////        prod2:=prevHead^prev2
////        prod3:=prev3^gfMul(prevHead,B"12'x002")
////
////
////        }
//
//
//
//    val out = Vec(Bits(GfBits bits),CorrectionNum)
//    ( 0 until(CorrectionNum)).foreach{i=>
//      out(i):=prod(i)
//    }
//    out




  }

  case class TestGfzero() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 1)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), 1)
    override val latency: Int = 1


    dataOut.payload := RegNext(Vec(gfOperators.gfInv(dataIn.payload.asBits)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class TestGfmul() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 2)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), 1)
    override val latency: Int = 1


    dataOut.payload := RegNext(Vec(gfOperators.gfMul(dataIn.payload(0), dataIn.payload(1))))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class TestGFMUL() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {

    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 2 * CorrectionNum)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), CorrectionNum)
    override val latency: Int = 1


    dataOut.payload := RegNext(Vec(gfOperators.GFMul(dataIn.payload)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }


}

object TestComponent {
  def main(args: Array[String]): Unit = {
    //test gfMul
    //    val testCase=Seq("0f22","0233").map(BigInt(_,16))
    //    val GoldenCase=Seq("3410").map(BigInt(_,10))
    //    doFlowPeekPokeTest("TestGf",gfOperators.TestGfmul(), Seq(testCase),GoldenCase)


        val testCase: Seq[BigInt] =Seq.fill(CorrectionNum*2)(0).zipWithIndex.map{
          case (out,in)=> BigInt(in+1)
        }

//        val GoldenCase=Seq("2051","1043","2053","5","2054","1","2096","21","2099","1",
//          "2108","5","2111","1","2240","85","2243","1","2252","5","2255","1","2288","21","2291",
//          "1","2300","5","2303","1","2816","341","2819","1","2828","5","2831","1","2864","21","2867",
//          "1","2876","5","2879","1","3008","85","3011","1","3020","5","3023","1","3056","21","3059",
//          "1","3068","5","3071","1","1042","1372").map(BigInt(_,10))
          val GoldenCase=Seq("531","1937","220","837","399","769","192","5","307","321","460","213","447","129","1072","933").map(BigInt(_,10))
    doFlowPeekPokeTest("TestGf", gfOperators.TestGFMUL(), Seq(testCase), GoldenCase)


  }
}



