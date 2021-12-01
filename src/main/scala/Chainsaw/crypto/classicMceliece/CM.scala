package Chainsaw.crypto.classicMceliece

import Chainsaw.crypto.symmetric.SM4Config
import Chainsaw.dspTest.{DSPTestable, doFlowPeekPokeTest}
import breeze.linalg.{InjectNumericOps, rand}
import org.scalatest.flatspec.AnyFlatSpec
import spinal._
import spinal.core._
import spinal.lib.{Delay, Flow, boolPimped, master, slave}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



object gfOperators {



  def gfIsZero(a: Bits): Bits = {
    //require(a.getBitsWidth == config.m )
    // model: all 1 or one 1?
    //val b=B(0,config.m+1 bits)
    //todo : 13 should be replace with m+1
    Mux(a.orR,B"13'x0000",B"13'x1fff")
  }

  def gfAdd(a:Bits,b:Bits):Bits ={
    a^b
  }

  def gfMul(a:Bits,b:Bits):Bits={
    val m=12
    var temp=B(0,m*2-1 bits) //UInt
    var inc=b
    (0 until b.getBitsWidth).foreach{ i=>
      temp=Mux(inc.lsb,temp^(a<<i).resize(m*2-1 bits),temp)
      inc = inc>>1
    }

    //temp.asBits
    //todo:add f into config
//    val f=Bits(config.m+1 bits)
//    f:=((config.m)->true,3->true,0->true,default->false)
//    var inc2=temp.reversed.take(23)
//    temp = temp.take(23)
//    var temp1=B(0,config.m*2-1 bits)
//    (22  until  config.m by -1).foreach{i=>
//      temp = Mux(inc2.lsb,temp^((f<<(i-(config.m+1))).resize(23 bits)),temp)
//      inc2 = inc2>>1
//    }
//
//    temp.take(config.m)

    //soft
    var t = temp & (B"23'x7fc000")
    temp =temp^(t>>9).resize(m*2-1)
    temp =temp^(t>>12).resize(m*2-1)

    t=temp & (B"23'x3000")
    temp = temp^(t>>9).resize(m*2-1)
    temp = temp^(t>>12).resize(m*2-1)

    temp.take(m)

  }

  def gfSq(a:Bits):Bits={
    gfMul(a,a)
  }

  def gfInv(a:Bits):Bits={
    var out=a

    out=gfSq(out)
    var tem11=gfMul(out,a)

    out=gfSq(tem11)
    out=gfSq(out)
    var tem1111=gfMul(out,tem11)

    out=gfSq(tem1111)
    out=gfSq(out)
    out=gfSq(out)
    out=gfSq(out)
    out=gfMul(out,tem1111)

    out=gfSq(out)
    out=gfSq(out)
    out=gfMul(out,tem11)

    out=gfSq(out)
    out=gfMul(out,a)

    out=gfSq(out)
    out
  }

  def gfFrac(a:Bits,b:Bits):Bits={
    gfMul(gfInv(a),b)
  }

  def GFMul(a:Vec[Bits]):Vec[Bits]={
    //todo: require t>4
    val prod=Vec(Bits(12 bits),t*2-1)
    val tmp =Vec(Vec(Bits(m bits),t),t)

    for (i<-0 until t){
      (0 until(t)).foreach(j=> tmp(i)(j):=gfMul(a(j),a(t+i)) )
    }
    var ortmp=mutable.Buffer(Bits( m bits))
    for ( i<-0 until(t)){
      ortmp.append(tmp(0)(i))
      (1 until  i+1).foreach{j=>
        ortmp.append( ortmp.last^tmp(j)(i-j))
      }
      prod(i):=ortmp.last
    }

    for (i<- t until(2*t-1)){
      ortmp.append(tmp(i-t+1)(t-1))
      ((i-(t-1)+1) until  t).foreach{j=>
        ortmp.append( ortmp.last^tmp(j)(i-j))
      }
      prod(i):=ortmp.last
    }


    val mul2=Vec(Bits( m bits),t+2)
    (2 until t-1).foreach(i=> mul2(i):=prod(i)^gfMul(prod(i+t),2))
    mul2(t-1):=prod(t-1)
    mul2(t):=prod(t)
    mul2(t+1):=prod(t+1)

    val prod1=Vec(Bits(m bits),t+2)
    val prod2=Vec(Bits(m bits),t)
    for (i <- (t - 1) * 2 to 2*t-3 by -1) {
      prod1(i-t+3):=prod(i)^mul2(i-t+3)
      prod1(i-t+1):=prod(i)^mul2(i-t+1)
    }
    (0 until 2).foreach(i=> mul2(i):=prod(i)^gfMul(prod1(i+t),2))

    for (i<- 2*t-4 to t+2 by -1){
      prod2(i-t+3):=prod(i)^prod1(i-t+3)
      prod1(i-t+1):=prod(i)^mul2(i-t+1)
    }
    for (i<- t+1 to t by -1){
      prod2(i-t+3):=prod1(i)^prod1(i-t+3)
      prod1(i-t+1):=prod1(i)^mul2(i-t+1)
    }

    prod1(0):=mul2(0)
    prod2(2):=prod1(2)
    prod2(1):=prod1(1)
    prod2(0):=prod1(0)
    prod2

  }

  case class TestGfzero() extends Component with DSPTestable [Vec[Bits], Vec[Bits]]{
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 1)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), 1)
    override val latency: Int = 1


    dataOut.payload := RegNext(Vec(gfOperators.gfInv(dataIn.payload.asBits)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class TestGfmul() extends Component with DSPTestable [Vec[Bits], Vec[Bits]]{
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 2)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), 1)
    override val latency: Int = 1


    dataOut.payload := RegNext(Vec(gfOperators.gfFrac(dataIn.payload(0),dataIn.payload(1))))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class TestGFMUL() extends Component with DSPTestable [Vec[Bits],Vec[Bits]]{

    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(12 bits), 2*t)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), t)
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

    var a="0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63".replace(", ", "\",\"")
    //println(a)
    //a=(+a)
//    val testCase=Seq("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63",
//      "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64").map(BigInt(_,10))
//    val GoldenCase=Seq("3685").map(BigInt(_,10))
    val testCase=Seq("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15", "16",//"1","2","3","4"//,
      "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"
    ).map(BigInt(_,10))
    val GoldenCase=Seq("10","6","25","13").map(BigInt(_,10))
    doFlowPeekPokeTest("TestGf",gfOperators.TestGFMUL(), Seq(testCase),GoldenCase)
  }
}



