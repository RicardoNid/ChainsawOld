package Chainsaw.DSP.fir

import Chainsaw.DFG.FirArch._
import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.matlabIO.eng
import spinal.core._

import scala.language.postfixOps

object FIR {
  def main(args: Array[String]): Unit = {

    val size = 120

    val firTestCase: Seq[BigInt] = Seq.fill(120)(BigInt(0)) ++ (0 until 100).map(_ => DSPRand.nextBigInt(12))
    val coeffs = (0 until 120).map(_ => DSPRand.nextInt(1 << 12))
    val firGolden: Array[Int] = eng.feval("filter", coeffs.reverse.map(_.toDouble).toArray, Array(1), firTestCase.map(_.toDouble).toArray)
      .asInstanceOf[Array[Double]].map(_.toInt)

    //    val mac = (coeff: SInt, data: SInt, partialSum: SInt) =>
    //      RegNext(coeff * data) + partialSum
    //    val macNode = TrinaryNode(mac, "mac", delay = 1 cycles)

    val mac = (coeff: SInt, data: SInt, partialSum: SInt) => (coeff * data) + partialSum
    val macNode = TrinaryNode(mac, "mac", delay = 0 cycles)


//    val firDFG = FIRGen[SInt, Int](macNode, TRANSPOSE, coeffs, 18 bits, 1)
    val firDFG = FIRGen[SInt, Int](macNode, SYSTOLIC, coeffs, 18 bits, 1)

    //    synthDSPNode[SInt](firDFG.getGraphAsNode, Seq(27 bits))
    testDSPNode[SInt, Seq[BigInt], Int](firDFG.getGraphAsNode(), Seq(27 bits), firTestCase.map(Seq(_)), firGolden, initLength = size)
    implDSPNode[SInt](firDFG.getGraphAsNode(), Seq(27 bits), forTiming = true)
    //    implDSPNode[SInt](firDFG.getGraphAsNode(), Seq(27 bits), forTiming = false)
  }
}
