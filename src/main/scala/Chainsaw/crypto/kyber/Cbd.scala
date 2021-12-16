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
    val Goldenvalue1 =Seq("0","0","1","0","1","0","2","0","-1","0","0","0","0","0","1","0","-1","0","0","0","0","0","1","0","-2","0","-1","0","-1","0","0","0","0","1","1","1","1","1","2","1","-1","1","0","1","0","1","1","1","-1","1","0","1","0","1","1","1","-2","1","-1","1","-1","1","0","1","0","1","1","1","1","1","2","1","-1","1","0","1","0","1","1","1","-1","1","0","1","0","1","1","1","-2","1","-1","1","-1","1","0","1","0","2","1","2","1","2","2","2","-1","2","0","2","0","2","1","2","-1","2","0","2","0","2","1","2","-2","2","-1","2","-1","2","0","2","0","-1","1","-1","1","-1","2","-1","-1","-1","0","-1","0","-1","1","-1","-1","-1","0","-1","0","-1","1","-1","-2","-1","-1","-1","-1","-1","0","-1","0","0","1","0","1","0","2","0","-1","0","0","0","0","0","1","0","-1","0","0","0","0","0","1","0","-2","0","-1","0","-1","0","0","0","0","0","1","0","1","0","2","0","-1","0","0","0","0","0","1","0","-1","0","0","0","0","0","1","0","-2","0","-1","0","-1","0","0","0","0","1","1","1","1","1","2","1","-1","1","0","1","0","1","1","1","-1","1","0","1","0","1","1","1","-2","1","-1","1","-1","1","0","1").map(BigInt(_,10))
    val Goldenvalue2 =Seq("0","1","-1","0","2","-1","-1","1","2","-1","0","1","0","-2","-2","1","0","-1","-1","2","2","0","0","1","0","0","1","2","1","-2","-1","2","-2","0","0","2","0","-2","0","3","0","-2","1","-1","0","-1","-1","-1","0","0","0","0","2","-1","0","0","-1","-1","1","1","0","-3","-1","1","-2","1","1","0","0","-1","1","1","0","-1","2","1","-2","-2","0","1","-2","-1","1","2","0","0","0","-1","1","1","1","0","2","-1","-1","0","-1","1","0","0","1","-1","0","1","1","-1","1","0","0","0","0","0","0","1","1","1","2","0","1","1","-1","0","2","2","0","-2","0","2","-1","2","1","-2","1","0","1","-1","1","0","2","-1","-1","-1","0","-1","-1","0","1","0","1","1","2","-1","-1","1","3","0","0","-1","1","0","-3","1","2","0","-1","-1","2","1","-1","-1","3","-1","1","0","-3","-1","1","1","-2","0","3","0","-2","0","0","0","-1","1","1","-2","-3","1","-1","2","-1","0","1","0","-1","1","1","0","0","1","-1","-1","-2","1","-1","0","-1","2","1","1","-1","-2","0","1","0","-1","1","-1","-2","-1","-2","1","-1","-1","0","-1","-1","0","0","-1","0","-1","-1","0","-1","-1","-1","1","0","0","1","0","0","0","-2","0","1","1","-1","-2","-1","1").map(BigInt(_,10))
    doFlowPeekPokeTest("testCbd2",Cbd.Cbd2TestComponent(),Seq(testvalue),Goldenvalue1)
    doFlowPeekPokeTest("testCbd3",Cbd.Cbd3TestComponent(),Seq(testvalue2),Goldenvalue2)


  }
}