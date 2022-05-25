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


object Cbd {

  def cbd2(buf:Array[UInt]): Array[SInt] ={
    require(buf.forall(_.getBitsWidth==8)&&buf.size==128)
    val bs: Array[Bits] = buf.map(_.asBits).reverse.reduce(_##_).subdivideIn(4 bits).toArray
      bs.map( b => (b & 0x1).asSInt + (b & 0x2).rotateRight(1).asSInt - (b & 0x4).rotateRight(2).asSInt - (b & 0x8).rotateRight(3).asSInt   ).map(_.resize(16 bits))//
    //bs.map( b => b(0).asSInt + b(1).asSInt- b(2).asSInt - b(3).asSInt ).map(_.resize(16 bits))//
  }

  def cbd3(buf:Array[UInt]): Array[SInt] ={
    require(buf.forall(_.getBitsWidth==8)&&buf.size==192)
    val bs: Array[Bits] = buf.map(_.asBits).reverse.reduce(_##_).subdivideIn(6 bits).toArray
    bs.map{b=>
      val aa =(b & 0x1).asSInt + (b & 0x2).rotateRight(1).asSInt+ (b & 0x4).rotateRight(2).asSInt
      val bb =(b & 0x8).rotateRight(3).asSInt + (b & 0x10).rotateRight(4).asSInt+ (b & 0x20).rotateRight(5).asSInt
//      val aa =b(0).asSInt + b(1).asSInt+ b(2).asSInt
//      val bb =b(3).asSInt + b(4).asSInt+ b(5).asSInt
      (aa-bb).resize(16 bits)
    }
  }

  //now
  case class cbd3final() extends  Component {
     val dataIn = slave Flow Bits(24 bits)    //6*4
     val dataOut1 = master Flow Bits(24 bits)   //12*2
     val dataOut2 = master Flow Bits(24 bits)

    val begin = Reg(Bool())init(False)
    // val count = Counter(0,255)
    val res = Vec(Bits(12 bits),4)

    (0 until 24/6).foreach{ i=>
      val a0 = dataIn.payload(6*i+0).asUInt +^ dataIn.payload(6*i+1).asUInt + dataIn.payload(6*i+2).asUInt
      val b0 = dataIn.payload(6*i+3).asUInt +^ dataIn.payload(6*i+4).asUInt + dataIn.payload(6*i+5).asUInt
      val res0 = Mux(a0<b0,a0 + 3329-b0,a0-b0)
      res(i) := res0.asBits
    }
    begin:= dataIn.valid
    dataOut1.payload:=RegNext(res.take(2).reverse.reduce(_##_))
    dataOut2.payload:=RegNext(res.takeRight(2).reverse.reduce(_##_))
    dataOut1.valid:=begin
    dataOut2.valid:= begin

  }
  case class cbd2final() extends  Component {
    val dataIn = slave Flow Bits(8 bits)    //6*4
    val dataOut = master Flow Bits(24 bits)   //12*2

    val begin = Reg(Bool())init(False)
    // val count = Counter(0,255)
    val res = Vec(Bits(12 bits),2)

    (0 until 8/4).foreach{ i=>
      val a0 = dataIn.payload(4*i+0).asUInt +^ dataIn.payload(4*i+1).asUInt
      val b0 = dataIn.payload(4*i+2).asUInt +^ dataIn.payload(4*i+3).asUInt
      val res0 = Mux(a0<b0,a0 + 3329-b0,a0-b0)
      res(i) := res0.asBits
    }
    begin:= dataIn.valid
    dataOut.payload:=RegNext(res.take(2).reverse.reduce(_##_))
    dataOut.valid:=begin


  }

  case class cbd2finalCopy() extends  Component {
    val dataIn = slave Flow Bits(16 bits)    //6*4
    val dataOut1 = master Flow Bits(24 bits)   //12*2
    val dataOut2 = master Flow Bits(24 bits)

    val begin = Reg(Bool())init(False)
    // val count = Counter(0,255)
    val res = Vec(Bits(12 bits),4)

    (0 until 16/4).foreach{ i=>
      val a0 = dataIn.payload(4*i+0).asUInt +^ dataIn.payload(4*i+1).asUInt
      val b0 = dataIn.payload(4*i+2).asUInt +^ dataIn.payload(4*i+3).asUInt
      val res0 = Mux(a0<b0,a0 + 3329-b0,a0-b0)
      res(i) := res0.asBits
    }
    begin:= dataIn.valid
    dataOut1.payload:=RegNext(res.take(2).reverse.reduce(_##_))
    dataOut2.payload:=RegNext(res.takeRight(2).reverse.reduce(_##_))
    dataOut1.valid:=begin
    dataOut2.valid:= begin

  }
  //eta=3

  case class Cbd2TestComponent() extends Component with DSPTestable [Vec[UInt], Vec[SInt]]{
    override val dataIn: Flow[Vec[UInt]] = slave Flow Vec(UInt(8 bits), 128)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 256)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(cbd2(dataIn.payload.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class Cbd3TestComponent() extends Component with DSPTestable [Vec[UInt], Vec[SInt]]{
    override val dataIn: Flow[Vec[UInt]] = slave Flow Vec(UInt(8 bits), 192)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 256)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(cbd3(dataIn.payload.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }



}


object TestCbd{
  def main(args: Array[String]): Unit = {
    val testvalue = Seq.fill(128)(0).zipWithIndex.map{
      case(i,kk) => BigInt(kk)
    }
    val testvalue2=Seq.fill(192)(0).zipWithIndex.map{
      case(i,kk) => BigInt(kk)
    }
    val Goldenvalue1 =Seq("0","0","1","0","1","0","2","0","-1","0","0","0","0","0","1","0","-1","0","0","0","0","0","1","0","-2","0","-1","0","-1","0","0","0","0","1","1","1","1","1","2","1","-1","1","0","1","0","1","1","1","-1","1","0","1","0","1","1","1","-2","1","-1","1","-1","1","0","1","0","1","1","1","1","1","2","1","-1","1","0","1","0","1","1","1","-1","1","0","1","0","1","1","1","-2","1","-1","1","-1","1","0","1","0","2","1","2","1","2","2","2","-1","2","0","2","0","2","1","2","-1","2","0","2","0","2","1","2","-2","2","-1","2","-1","2","0","2","0","-1","1","-1","1","-1","2","-1","-1","-1","0","-1","0","-1","1","-1","-1","-1","0","-1","0","-1","1","-1","-2","-1","-1","-1","-1","-1","0","-1","0","0","1","0","1","0","2","0","-1","0","0","0","0","0","1","0","-1","0","0","0","0","0","1","0","-2","0","-1","0","-1","0","0","0","0","0","1","0","1","0","2","0","-1","0","0","0","0","0","1","0","-1","0","0","0","0","0","1","0","-2","0","-1","0","-1","0","0","0","0","1","1","1","1","1","2","1","-1","1","0","1","0","1","1","1","-1","1","0","1","0","1","1","1","-2","1","-1","1","-1","1","0","1"
    ).map(BigInt(_,10))
    val Goldenvalue2 =Seq("0","1","-1","0","2","-1","-1","1","2","-1","0","1","0","-2","-2","1","0","-1","-1","2","2","0","0","1","0","0","1","2","1","-2","-1","2","-2","0","0","2","0","-2","0","3","0","-2","1","-1","0","-1","-1","-1","0","0","0","0","2","-1","0","0","-1","-1","1","1","0","-3","-1","1","-2","1","1","0","0","-1","1","1","0","-1","2","1","-2","-2","0","1","-2","-1","1","2","0","0","0","-1","1","1","1","0","2","-1","-1","0","-1","1","0","0","1","-1","0","1","1","-1","1","0","0","0","0","0","0","1","1","1","2","0","1","1","-1","0","2","2","0","-2","0","2","-1","2","1","-2","1","0","1","-1","1","0","2","-1","-1","-1","0","-1","-1","0","1","0","1","1","2","-1","-1","1","3","0","0","-1","1","0","-3","1","2","0","-1","-1","2","1","-1","-1","3","-1","1","0","-3","-1","1","1","-2","0","3","0","-2","0","0","0","-1","1","1","-2","-3","1","-1","2","-1","0","1","0","-1","1","1","0","0","1","-1","-1","-2","1","-1","0","-1","2","1","1","-1","-2","0","1","0","-1","1","-1","-2","-1","-2","1","-1","-1","0","-1","-1","0","0","-1","0","-1","-1","0","-1","-1","-1","1","0","0","1","0","0","0","-2","0","1","1","-1","-2","-1","1"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testCbd2",Cbd.Cbd2TestComponent(),Seq(testvalue),Goldenvalue1)
    //doFlowPeekPokeTest("testCbd3",Cbd.CBD3(),Seq(testvalue2),Goldenvalue2)
    //VivadoSynth(Cbd.cbd3final())
    val myTest = Seq("020100","050403","080706","0B0A09").map(BigInt(_,16))
    SimConfig.withFstWave.compile(new Cbd.cbd3final).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      var idx = 0
      dut.dataIn.valid#=false

      sleep(1)
      while(idx<1000)
      {
        if(idx<4){
          dut.dataIn.valid #= true
          dut.dataIn.payload #= myTest(idx)
        }
        else{
          dut.dataIn.valid #= false
        }
        idx += 1
        sleep(10)
      }
    }
    val myTest2 = Seq("0100","0302","0504","0706","0908","0B0A","0D0C","0F0E","DDFF","ABCD","F897").map(BigInt(_,16))
    SimConfig.withFstWave.compile(new Cbd.cbd2final).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      var idx = 0
      dut.dataIn.valid#=false

      sleep(1)
      while(idx<1000)
      {
        if(idx<10){
          dut.dataIn.valid #= true
          dut.dataIn.payload #= myTest2(idx)
        }
        else{
          dut.dataIn.valid #= false
        }
        idx += 1
        sleep(10)
      }
    }


  }
}