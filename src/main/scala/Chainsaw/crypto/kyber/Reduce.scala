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

object Reduce {
  /*montgomeryReduce
   *Given a 32-bit integer,Computes 16-bit integer to a*R^-1 mod q
   *return {-q+1,...,q-1}
   */
  def montgomeryReduce(a: SInt): SInt = {
    //require(a.getWidth==32)
    var u: SInt = (62209 * a).resize(16)
    val t: SInt = ((a - u * q) >> 16).resize(16)
    t
  }

  /*barrettReduce
   *Given a 16-bit integer,Computes 16-bit integer to a mod q
   *return {0,...,q}
   */
  //todo: think about SInt
  def barrettReduce(a: SInt): SInt = {
    require(a.getWidth == 16)
    val v: SInt = ((1      << 26) + q / 2) / q
    val t: SInt = ((v * a) >> 26) * q
    (a - t).resize(16)
  }

  //a-q if a>= q else a
  def csubq(a: SInt): SInt = {
    require(a.getBitsWidth == 16)
    val b: SInt = (a - q) + (((a - q) |>> 15) & q)
    b
  }

  case class MonReduceTest() extends Component with DSPTestable[Vec[SInt], Vec[SInt]] {
    override val dataIn: Flow[Vec[SInt]]  = slave Flow Vec(SInt(16 bits), 1)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 1)
    override val latency: int32           = 1

    dataOut.payload := RegNext(Vec(Reduce.montgomeryReduce(dataIn.payload(0))))
    dataOut.valid   := Delay(dataIn.valid, latency, init = False)
  }

  case class BarReduceTest() extends Component with DSPTestable[Vec[SInt], Vec[SInt]] {
    override val dataIn: Flow[Vec[SInt]]  = slave Flow Vec(SInt(16 bits), 1)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 1)
    override val latency: int32           = 1

    dataOut.payload := RegNext(Vec(Reduce.barrettReduce(dataIn.payload(0))))
    dataOut.valid   := Delay(dataIn.valid, latency, init = False)
  }
  case class csubqReduceTest() extends Component with DSPTestable[Vec[SInt], Vec[SInt]] {
    override val dataIn: Flow[Vec[SInt]]  = slave Flow Vec(SInt(16 bits), 1)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 1)
    override val latency: int32           = 1

    dataOut.payload := RegNext(Vec(Reduce.csubq(dataIn.payload(0))))
    dataOut.valid   := Delay(dataIn.valid, latency, init = False)
  }

  def main(args: Array[String]): Unit = {

    val res1 = barrettReduce(-3322)
    val res2 = montgomeryReduce(-3330)
    println(res1, res2)

  }

}

object TestReduce {
  def main(args: Array[String]): Unit = {
    val testvalue    = Seq("-3").map(BigInt(_, 10))
    val Goldenvalue1 = Seq("-3").map(BigInt(_, 10))
    val Goldenvalue2 = Seq("-507").map(BigInt(_, 10))
    val Goldenvalue3 = Seq("3326").map(BigInt(_, 10))

    doFlowPeekPokeTest("testCsubq", Reduce.csubqReduceTest(), Seq(testvalue), Goldenvalue1)

    doFlowPeekPokeTest("testMon", Reduce.MonReduceTest(), Seq(testvalue), Goldenvalue2)

    doFlowPeekPokeTest("testBar", Reduce.BarReduceTest(), Seq(testvalue), Goldenvalue3)
  }
}
