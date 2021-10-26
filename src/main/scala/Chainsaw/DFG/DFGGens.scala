package Chainsaw.DFG

import Chainsaw._
import spinal.core._

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

    // allocate the coeff to mults, this is the same for all architectures
    firType match {
      case DIRECT =>
        mults.zip(consts.reverse).foreach { case (mult, coeff) => dfg.addEdge(coeff(0), mult(0), 0) }
        mults.zipWithIndex.foreach { case (mult, i) => dfg.addEdge(input(0), mult(1), i) }
        mults.tail.zip(adds).foreach { case (mult, add) => dfg.addEdge(mult(0), add(0), 0) }
        (mults.head +: adds.init).zip(adds).foreach { case (prev, next) => dfg.addEdge(prev(0), next(1), 0) }
      case TRANSPOSE =>
        mults.zip(consts).foreach { case (mult, coeff) => dfg.addEdge(coeff(0), mult(0), 0) }
        mults.foreach(mult => dfg.addEdge(input(0), mult(1), 0))
        mults.tail.zip(adds).foreach { case (mult, add) => dfg.addEdge(mult(0), add(0), 0) }
        (mults.head +: adds.init).zip(adds).foreach { case (prev, next) => dfg.addEdge(prev(0), next(1), 1) }
      case SYSTOLIC =>
    }
    dfg.setOutput(adds.last)
    dfg
  }

  def main(args: Array[String]): Unit = {

  }
}
