package Chainsaw.crypto.kyber

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.crypto.symmetric._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object Keccak {

  def kState: Array[Bits] = Array(
    "0000000000000001",
    "0000000000008082",
    "800000000000808a",
    "8000000080008000",
    "000000000000808b",
    "0000000080000001",
    "8000000080008081",
    "8000000000008009",
    "000000000000008a",
    "0000000000000088",
    "0000000080008009",
    "000000008000000a",
    "000000008000808b",
    "800000000000008b",
    "8000000000008089",
    "8000000000008003",
    "8000000000008002",
    "8000000000000080",
    "000000000000800a",
    "800000008000000a",
    "8000000080008081",
    "8000000000008080",
    "0000000080000001",
    "8000000080008008"
   ).map(BigInt(_, 16)).map(B(_, 64 bits))

  //val rXYValue = Seq(0, 1, 62, 28, 27, 36, 44, 6, 55, 20, 3, 10, 43, 25, 39, 41, 45, 15, 21, 8, 18, 2, 61, 56, 14)
  val rXYValue = Seq(0, 36, 3, 41, 18, 1, 44, 10, 45, 2, 62, 6, 43, 15, 61, 28, 55, 25, 21, 56, 27, 20, 39, 8, 14)
  def theta(A: Array[Array[Bits]]): Array[Array[Bits]] = {
    require(A.forall(_.forall(_.getBitsWidth == 64))) // check bit width
    // theta1, row-wise reduce XOR
    val B: Array[Bits] = A.map(_.reduce(_ ^ _))
    //theta2
    val C =Array.tabulate(5)(x=>B((x+4)%5)^B((x+1)%5).rotateLeft(1))
    //theta3
    A.zip(C).map { case (a, c) => a.map(_ ^ c) }
  }

  def rou(A: Array[Array[Bits]]): Array[Array[Bits]] =
    A.flatten.zip(rXYValue).map { case (bits, i) => bits.rotateLeft(i) }.grouped(5).toArray

  def pi(A: Array[Array[Bits]]): Array[Array[Bits]] =
    Array.tabulate(5, 5)((x, y) => A((x + 3 * y) % 5)(x))


  def chi(A: Array[Array[Bits]]): Array[Array[Bits]]
  = Array.tabulate(5, 5)((x, y) => A(x)(y) ^ (~A((x + 1) % 5)(y) & A((2 + x) % 5)(y)))

  def iota(A: Array[Array[Bits]], state: Bits): Array[Array[Bits]] =
    ((A.flatten.head ^ state) +: A.flatten.tail).grouped(5).toArray


  def keccakF(state: Array[Bits]): Array[Bits] = {
    require(state.forall(_.getBitsWidth == 64) && state.size == 25)
    val statex: Array[Bits] =(state.grouped(5).toArray).transpose.flatten
    //val statex: Array[Bits] =Array.tabulate(5,5)((x, y)=>stateT(y)(x)).flatten
    val AForEachRound = ArrayBuffer[Array[Bits]](statex)

    (0 until 24).foreach{i =>
      val A0: Array[Array[Bits]] = AForEachRound.last.grouped(5).toArray
      val A1 = theta(A0)
      val A2 = rou(A1)
      val A3 = pi(A2)
      val A4 = chi(A3)
      val A5 = iota(A4, kState(i))
      val ret = A5.flatten
      A5.transpose.flatten.zipWithIndex.foreach{ case (bits, j) => bits.setName(s"round_${i}_${j}")}
      AForEachRound += A5.flatten
    }
    AForEachRound.last.grouped(5).toArray.transpose.flatten
  }

  //Absorb
//  def keccakAbsorb(r : SInt, m: Array[Bits],mlen:SInt, p:Bits): Array[Bits] ={
//    require(m.forall(_.getBitsWidth==8)&&(p.getBitsWidth==8))
//    val s0 =Array.fill(25)(BigInt(0)).map(B(_,64 bits))
//    val C: Array[Bits] = m.grouped(8).map(_.reduce(_ ## _)).toArray
//    val s = ArrayBuffer[Array[Bits]](s0)
//    val ml= ArrayBuffer[SInt](mlen)
//    when( ml.last>=r){
//      val s1: Array[Bits] =s.last.zip(C).map{case(ss,c)=>  ss^c }//load64?
//      val s2 = keccakF(s1)
//      s +=s2
//      val mlen= ml.last-r
//      ml += mlen
//    }
//
//  }
  //SHA3_516



  case class TestComponent() extends Component with DSPTestable [Vec[Bits], Vec[Bits]]{
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(64 bits), 25)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(64 bits), 25)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.keccakF(dataIn.payload.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }
}

object TestKeccak{
  def main(args: Array[String]): Unit = {
    //val testeCase =Seq.fill(25)(BigInt(0))
    val testCase = Seq("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24").map(BigInt(_,10))
//    GenRTL(Keccak.TestComponent(), name = "keccakF")
    val GoldenCase = Seq(
      "9472389783892099349",
     "2159377575142921216",
     "17826682512249813373",
     "2325963263767348549",
     "15086930817298358378",
     "11661812091723830419",
     "3517755057770134847",
     "5223775837645169598",
     "933274647126506074",
     "3451250694486589320",
     "825065683101361807",
     "6192414258352188799",
     "14426505790672879210",
     "3326742392640380689",
     "16749975585634164134",
     "17847697619892908514",
     "11598434253200954839",
     "6049795840392747215",
     "8610635351954084385",
     "18234131770974529925",
     "15330347418010067760",
     "12047099911907354591",
     "4763389569697138851",
     "6779624089296570504",
     "15083668107635345971").map(BigInt(_,10))

    doFlowPeekPokeTest("testKeccakF",Keccak.TestComponent(), Seq(testCase),GoldenCase)
  }
}
