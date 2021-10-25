package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class DFGGensTest extends AnyFlatSpec {

  import Operators._
  import FirType._

  "fir structure" should "be correct as fir" in {

    val add = SIntAdder("add", 20 bits, 0 cycles)
    val mult = SIntMult("mult", 10 bits, 0 cycles)
    val coeffs = 0 until 10
    val firDirect = DFGGens.fir(add, mult, DIRECT, coeffs, 4 bits)
    println(firDirect)
    //

    val testCase = (0 until 100).map(_ => DSPRand.nextBigInt(2))
    val golden = eng.feval("filter", coeffs.reverse.map(_.toDouble).toArray, Array(1), testCase.map(_.toDouble).toArray).asInstanceOf[Array[Double]]
    //    println(golden.map(_.toInt).mkString(" "))
    //
    //    doFlowPeekPokeTest(new Component with DSPTestable[SInt, SInt] {
    //      val dataIn: Flow[SInt] = slave Flow SInt(10 bits)
    //      val dataOut: Flow[SInt] = master Flow SInt()
    //      val latency = 0
    //      dataOut.valid := Delay(dataIn.valid, latency, init = False)
    //      dataOut.payload := firDirect.impl(Seq(dataIn.payload)).head
    //    }, "testFir", testCase, golden.map(_.toInt))
  }

}
