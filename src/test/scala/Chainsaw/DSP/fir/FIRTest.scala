package Chainsaw.DSP.fir

import Chainsaw.DFG.FirArch._
import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.{SInt, _}

class FIRTest extends AnyFlatSpec {

  val doSynth = false

  val testSize = 120
  val coeffWidth: BitCount = 18 bits
  val coeffs: Seq[Int] = (0 until 120).map(_ => DSPRand.nextInt(1 << 12))

  // preparing testcase and golden
  val firTestCase: Seq[BigInt] = Seq.fill(testSize)(BigInt(0)) ++ (0 until 100).map(_ => DSPRand.nextBigInt(12))
  val firGolden: Array[Int] = eng.feval("filter",
    coeffs.reverse.map(_.toDouble).toArray, Array(1),
    firTestCase.map(_.toDouble).toArray)
    .asInstanceOf[Array[Double]].map(_.toInt)

  val pipelinedMAC: TrinaryNode[SInt] = Operators.macDSP48(mreg = true)
  val mac: TrinaryNode[SInt] = Operators.macDSP48(mreg = false)

  "FIR" should "work correctly under different arch and pipelining strategy" in {

    val archs = Seq(DIRECT, TRANSPOSE, SYSTOLIC)
    val nodes = Seq(pipelinedMAC, mac)

    Seq.tabulate(3, 2) { (i, j) =>
      val dfgGen = FIRGen(nodes(j), archs(i), coeffs, coeffWidth, 1)
      val dfg: DFGGraph[SInt] = dfgGen.getGraph
      testDFG(dfg, dfgGen.latency cycles, Seq(27 bits), firTestCase.map(Seq(_)), firGolden, initLength = testSize)
      if(doSynth) {
        implDFG(dfg, Seq(27 bits), forTiming = false)
      }
    }
  }



}
