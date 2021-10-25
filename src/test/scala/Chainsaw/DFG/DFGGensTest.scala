package Chainsaw.DFG

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._

class DFGGensTest extends AnyFlatSpec {

  import FirType._

  "fir structure" should "be correct as fir" in {

    val addOp = (a: SInt, b: SInt) => a + b
    val multOp = (a: SInt, b: SInt) => a * b
    val add = BinaryNode(addOp, "add", 10 bits)
    val mult = BinaryNode(multOp, "mult", 20 bits)
    val coeffs = 0 until 10
    val firDirect = DFGGens.fir(add, mult, DIRECT, coeffs, 5 bits)

    val testCase = (0 until 100).map(_ => DSPRand.nextBigInt(2))
    val golden = eng.feval("filter", coeffs.reverse.map(_.toDouble).toArray, Array(1), testCase.map(_.toDouble).toArray).asInstanceOf[Array[Double]]

    doFlowPeekPokeTest(new Component with DSPTestable[SInt, SInt] {
      val dataIn: Flow[SInt] = slave Flow SInt(10 bits)
      val dataOut: Flow[SInt] = master Flow SInt()
      val latency = 0
      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload := firDirect.impl(Seq(dataIn.payload)).head
    }, "testFir", testCase, golden.map(_.toInt))
  }

  "fir structure" should "be correct as convenc" in {

    val and = BinaryNode(Operators.and, "and")
    val xor = BinaryNode(Operators.xor, "xor")
    def convDirect(coeffs: Seq[Int]): DFGGraph[Bits] = DFGGens.fir(xor, and, DIRECT, coeffs, 1 bits)

    import Communication.channelCoding._
    val testCase = (0 until 100).map(_ => DSPRand.nextBigInt(1))
    val conv802_11: ConvConfig = ConvConfig(Array(171, 133), radix = 8)
    val trellisM = Refs.poly2trellisM(conv802_11.ms.map(_ + 1), conv802_11.codeGens)
    val golden: Array[Int] = Refs.convenc(testCase.map(_.toInt).toArray, trellisM)

    doFlowPeekPokeTest(new Component with DSPTestable[Vec[Bits], Vec[Bits]] {
      val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(1 bits), 1)
      val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(), 2)
      val latency = 0
      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload(0) := convDirect(BigInt("171", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)).impl(Seq(dataIn.payload).head).head
      dataOut.payload(1) := convDirect(BigInt("133", 8).toString(2).reverse.padTo(7, '0').map(_.asDigit)).impl(Seq(dataIn.payload).head).head
    }, "testConv", testCase.grouped(1).toSeq, golden)
  }
}
