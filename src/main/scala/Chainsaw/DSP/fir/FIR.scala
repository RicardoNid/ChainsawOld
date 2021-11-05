package Chainsaw.DSP.fir

import Chainsaw.DFG.FirArch._
import Chainsaw.DFG._
import Chainsaw._
import spinal.core._

object FIR {
  def main(args: Array[String]): Unit = {
    val coeffs = (0 until 120).map(_ => DSPRand.nextInt(1 << 17))
    val add = BinaryNode(Operators.sintAdd, "add")
    val mult = BinaryNode(Operators.sintMult, "mult")
    val firDFG = FIRGen[SInt, Int](add, mult, DIRECT, coeffs, 18 bits, 1)

    //    synthDSPNode[SInt](firDFG.getGraphAsNode, Seq(27 bits))
    implDSPNode[SInt](firDFG.getGraphAsNode, Seq(27 bits))
  }
}
