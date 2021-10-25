package Chainsaw.DFG

import Chainsaw._
import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object FirType extends Enumeration {
  type FirType = Value
  val DIRECT, TRANSPOSE, SYSTOLIC = Value
}

import Chainsaw.DFG.FirType._

object DFGGens {

  def fir[THard, TSoft](add: DSPNode[THard], mult: DSPNode[THard], const: (String, TSoft) => ConstantNode[THard],
                        coeffs: Seq[TSoft], firType: FirType): DFGGraph[THard] = {

    val dfg = DFGGraph[THard]()
    val size = coeffs.size
    val mults = (0 until size).map(i => mult.copy(s"${mult.name}_$i"))
    val adds = (0 until size - 1).map(i => add.copy(s"${add.name}_$i"))
    val consts = coeffs.zipWithIndex.map { case (soft, i) => const(s"const_$i", soft) }
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
    import Operators._
    val add = SIntAdder("add", 20 bits, 0 cycles)
    val mult = SIntMult("mult", 10 bits, 0 cycles)
    val constGen = (name: String, constant: Int) => SIntConst(name, constant, 10 bits)
    val firDirect = fir(add, mult, constGen, (0 until 10), DIRECT)
    println(firDirect)

    val coeffs = 0 until 10
    val testCase = (0 until 100).map(_ => DSPRand.nextBigInt(2))
    val golden = eng.feval("filter", coeffs.reverse.map(_.toDouble).toArray, Array(1), testCase.map(_.toDouble).toArray).asInstanceOf[Array[Double]]
    println(golden.map(_.toInt).mkString(" "))

    doFlowPeekPokeTest(new Component with DSPTestable[SInt, SInt] {
      val dataIn: Flow[SInt] = slave Flow SInt(10 bits)
      val dataOut: Flow[SInt] = master Flow SInt()
      val latency = 0
      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.payload := firDirect.impl(Seq(dataIn.payload)).head
    }, "testFir", testCase, golden.map(_.toInt))
  }
}
