package Chainsaw.crypto.kyber

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.crypto.symmetric._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.{breakOut, immutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object ParseDecode {

  // todo Do a timing design
  // IN :*buf ,bufLen ;  OUT: *r,len
  def parse(buf: Array[Bits], bufLen: Bits, len: Bits): (Array[Bits], Bits) = {
    val bufB: Array[Bits] = buf.map(_.asBits).reverse.reduce(_ ## _).subdivideIn(24 bits).toArray
    val lenArray = ArrayBuffer[UInt](0)
    val buflenArray = ArrayBuffer[UInt](0)
    val r = ArrayBuffer[Bits]()
    var x: SInt = 0
    var y = 0

    bufB.zipWithIndex.map { case (a, i) =>
      when((U(264) < U(1)) & (U(500) <= bufLen.asUInt)) {
        val Seq(x0, x1) = a.subdivideIn(12 bits)
        when(x0.asUInt < q) {
          r += x0
          x += 1
          lenArray.last.setName(s"last${i}_0")
        }

        when((x1.asUInt < q) & (lenArray.last < q)) {
          r += x1
          x += 1
        }
        y += 3
      }
    }
    (r.toArray.map(B(_, 16 bits)), lenArray.last.asBits)
  }

  case class paresTestComponent() extends Component with DSPTestable[Vec[Bits], Vec[SInt]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 408)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 272)
    override val latency: int32 = 1
    val (x, b) = parse(dataIn.payload.toArray, 408, 272)
    dataOut.payload := RegNext(Vec(x.map(_.asSInt)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  //  def main(args: Array[String]): Unit = {
  //    val testvalue = Seq.fill(384)(BigInt(0))
  //    val (x: Array[Bits],y)= parse(testvalue.toArray.map(B(_,8 bits)),360,256)
  //
  //    //println(x.size)
  //  }

}

object TestParse {
  def main(args: Array[String]): Unit = {
    val testvalue = Seq.fill(408)(BigInt(25)) //.zipWithIndex.map{
    //      case(a,i)=>BigInt(i)
    //    }
    val gold = Seq.fill(272)(BigInt(0)).zipWithIndex.map {
      case (a, i) => if (i < 100) (BigInt(i))
      else (BigInt(10))
    }
    val Goldenvalue1 = Seq("2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401", "2329", "401").map(BigInt(_, 10))
    doFlowPeekPokeTest("testCbd2", ParseDecode.paresTestComponent(), Seq(testvalue), Goldenvalue1)


  }
}
