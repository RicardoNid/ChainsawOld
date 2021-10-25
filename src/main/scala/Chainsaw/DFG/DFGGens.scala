package Chainsaw.DFG

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object FirType extends Enumeration {
  type FirType = Value
  val DIRECT, TRANSPOSE, SYSTOLIC = Value
}

import Chainsaw.DFG.FirType._

object DFGGens {

  def fir[THard <: Data, TSoft](add: DSPNode[THard], mult: DSPNode[THard], firType: FirType,
                                coeffs: Seq[TSoft], coeffWidth: BitCount)(implicit converter: (TSoft, BitCount) => THard): DFGGraph[THard] = {

    val dfg = DFGGraph[THard]()
    val size = coeffs.size
    val mults = (0 until size).map(i => mult.copy(s"${mult.name}_$i"))
    val adds = (0 until size - 1).map(i => add.copy(s"${add.name}_$i"))
    val consts = coeffs.zipWithIndex.map { case (soft, i) => ConstantNode[THard, TSoft](s"const_$i", soft, coeffWidth) }
    (mults ++ adds ++ consts).foreach(dfg.addVertices(_))
    val input = dfg.addInput("input")

    firType match {
      case DIRECT =>
        mults.zip(consts).foreach { case (mult, coeff) => dfg.addEdge(coeff(0), mult(0), 0) }
        mults.foreach(mult => dfg.addEdge(input(0), mult(1), 0))
        mults.tail.zip(adds).foreach { case (mult, add) => dfg.addEdge(mult(0), add(0), 0) }
        (mults.head +: adds.init).zip(adds).foreach { case (prev, next) => dfg.addEdge(prev(0), next(1), 1) }
        dfg.setOutput(adds.last)
      case TRANSPOSE =>
      case SYSTOLIC =>
    }

    dfg
  }

  import dspTest._

  def main(args: Array[String]): Unit = {

    val and = BinaryNode(Operators.and, "and")
    val xor = BinaryNode(Operators.xor, "xor")

    def convDirect(coeffs: Seq[Int]): DFGGraph[Bits] = fir(xor, and, DIRECT, coeffs, 1 bits)

    val testCase = (0 until 100).map(_ => DSPRand.nextBigInt(1))
    import Communication.channelCoding._
    val conv802_11: ConvConfig = ConvConfig(Array(171, 133), radix = 8)
    val trellisM = Refs.poly2trellisM(conv802_11.ms.map(_ + 1), conv802_11.codeGens)
    val golden: Array[Int] = Refs.convenc(testCase.map(_.toInt).toArray, trellisM)

    doFlowPeekPokeTest(new Component with DSPTestable[Bits, Vec[Bits]] {
      val dataIn: Flow[Bits] = slave Flow Bits(1 bits)
      val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(), 2)
      val latency = 0
      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload(0) := convDirect(BigInt("171",8).toString(2).reverse.padTo(7,'0').map(_.asDigit)).impl(Seq(dataIn.payload)).head
      dataOut.payload(1) := convDirect(BigInt("133",8).toString(2).reverse.padTo(7,'0').map(_.asDigit)).impl(Seq(dataIn.payload)).head
    }, "testConv", testCase, golden)


//    VivadoSynth(new Component with DSPTestable[Bits, Vec[Bits]] {
//      val dataIn: Flow[Bits] = slave Flow Bits(1 bits)
//      val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(), 2)
//      val latency = 0
//      dataOut.valid := Delay(dataIn.valid, latency, init = False)
//      dataOut.payload(0) := convDirect(Seq(1, 1, 0, 0, 0)).impl(Seq(dataIn.payload)).head
//      dataOut.payload(1) := convDirect(Seq(1, 0, 1, 0, 1)).impl(Seq(dataIn.payload)).head
//    })
  }
}
