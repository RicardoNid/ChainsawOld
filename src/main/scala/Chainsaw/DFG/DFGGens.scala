package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

object FirArch extends Enumeration {
  type FirArch = Value
  val DIRECT, TRANSPOSE, SYSTOLIC = Value
}

object FirType extends Enumeration {
  type FirType = Value
  val STATIC, DYNAMIC = Value
}

object FFTArch extends Enumeration {
  type FFTArch = Value
  val DIT, DIF = Value
}

import Chainsaw.DFG.FirArch._
import Chainsaw.DFG.FirType._
import Chainsaw.DFG.FFTArch._

object DFGGens {

  /** Generic method to generate an abstract FIR DFG
   */
  def fir[THard <: Data, TSoft](add: BinaryNode[THard], mult: BinaryNode[THard],
                                firArch: FirArch,
                                coeffs: Seq[TSoft], coeffWidth: BitCount, parallelism: Int = 1)
                               (implicit converter: (TSoft, BitCount) => THard): DFGGraph[THard] = {

    val dfg = DFGGraph[THard]("fir graph")
    val size = coeffs.size

    val mults = (0 until size).map(i => mult.copy(s"${mult.name}_$i"))
    val adds = (0 until size - 1).map(i => add.copy(s"${add.name}_$i"))
    val consts = coeffs.zipWithIndex.map { case (soft, i) => ConstantNode[THard, TSoft](s"const_$i", soft, coeffWidth) }
    (mults ++ adds ++ consts).foreach(dfg.addVertices(_))
    val input = dfg.addInput("input")

    // allocate the coeff to mults, this is the same for all architectures
    firArch match {
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
        mults.zip(consts.reverse).foreach { case (mult, coeff) => dfg.addEdge(coeff(0), mult(0), 0) }
        mults.zipWithIndex.foreach { case (mult, i) => dfg.addEdge(input(0), mult(1), 2 * i) }
        mults.tail.zip(adds).foreach { case (mult, add) => dfg.addEdge(mult(0), add(0), 0) }
        (mults.head +: adds.init).zip(adds).foreach { case (prev, next) => dfg.addEdge(prev(0), next(1), 1) }
    }
    dfg.setOutput(adds.last)

    // folding/unfolding according to the pattern
    val ret = if (parallelism == 1) dfg
    else if (parallelism > 1) new Unfolding(dfg, parallelism).unfolded
    else {
      val foldingFactor = -parallelism
      val multGroups = mults.grouped(-parallelism).toSeq.map(_.padTo(foldingFactor, null))
      val addGroups = adds.grouped(-parallelism).toSeq.map(_.padTo(foldingFactor, null))
      val foldingSet = multGroups ++ addGroups
      DFGTestUtil.verifyFolding(dfg.asInstanceOf[DFGGraph[SInt]], foldingSet.asInstanceOf[Seq[Seq[BinaryNode[SInt]]]])
      new Folding(dfg, foldingSet).folded
    }
    logger.debug(s"fir dfg:\n$ret")
    ret
  }


  /** Specific implementation of FIR DFG in real field (\R), using MAC module like DSP slice
   */
  def firDSP[THard <: Data, TSoft](multAdd: TrinaryNode[THard],
                                   firArch: FirArch,
                                   coeffs: Seq[TSoft], coeffWidth: BitCount, parallelism: Int = 1)
                                  (implicit converter: (TSoft, BitCount) => THard): DFGGraph[THard] = {

    val dfg = DFGGraph[THard]("fir graph")
    val size = coeffs.size

    val multAdds = (0 until size).map(i => multAdd.copy(s"${multAdd.name}_$i"))
    val consts = coeffs.zipWithIndex.map { case (soft, i) => ConstantNode[THard, TSoft](s"const_$i", soft, coeffWidth) }
    val zero = ConstantNode[THard, TSoft](s"zero", 0.asInstanceOf[TSoft], coeffWidth) // TODO: not generic
    (multAdds ++ consts :+ zero).foreach(dfg.addVertices(_))
    val input = dfg.addInput("input")

    // allocate the coeff to mults, this is the same for all architectures
    firArch match {
      case DIRECT =>
        multAdds.zip(consts.reverse).foreach { case (multAdd, coeff) => dfg.addEdge(coeff(0), multAdd(0), 0) } // A
        multAdds.zipWithIndex.foreach { case (mult, i) => dfg.addEdge(input(0), mult(1), i) } // B
        multAdds.init.zip(multAdds.tail).foreach { case (prev, next) => dfg.addEdge(prev(0), next(2), 0) } // C
        dfg.addEdge(zero(0), multAdds.head(2), 0)
      case TRANSPOSE =>
        multAdds.zip(consts).foreach { case (multAdd, coeff) => dfg.addEdge(coeff(0), multAdd(0), 0) } // A
        multAdds.zipWithIndex.foreach { case (mult, i) => dfg.addEdge(input(0), mult(1), 0) } // B
        multAdds.init.zip(multAdds.tail).foreach { case (prev, next) => dfg.addEdge(prev(0), next(2), 1) } // C
        dfg.addEdge(zero(0), multAdds.head(2), 1)
    }
    dfg.setOutput(multAdds.last)

    // folding/unfolding according to the pattern
    val ret = if (parallelism == 1) dfg
    else if (parallelism > 1) new Unfolding(dfg, parallelism).unfolded
    else {
      val foldingFactor = -parallelism
      val multAddGroups = multAdds.grouped(-parallelism).toSeq.map(_.padTo(foldingFactor, null))
      DFGTestUtil.verifyFolding(dfg.asInstanceOf[DFGGraph[SInt]], multAddGroups.asInstanceOf[Seq[Seq[TrinaryNode[SInt]]]])
      new Folding(dfg, multAddGroups).folded
    }
    logger.debug(s"fir dfg:\n$ret")
    ret
  }


  /** Generic method to generate an abstract radix-2 FFT DFG, this method use a "butterfly core" as its building block
   * @param butterfly: the implementation of the butterfly core
   * @param size: size of the FFT DFG, should be a power of 2
   * @param fftArch: architecture of fft, could be DIT or DIF
   * @param coeffGen: a generator to generate the twiddle factor w^ik^ on the specific field
   * @param coeffWidth: bit width of the coefficients
   * @param parallelism: determining the parallelism of the design, compared to the fully-pipelined version
   * @example parallelism = 3 -> unfold 3; parallelism = -3 -> fold 3
   */
  def radix2fft[THard <: Data, TSoft](butterfly: ButterflyNode[THard], size:Int,
                                      fftArch: FFTArch,
                                      coeffGen: Int => TSoft, coeffWidth: BitCount, parallelism: Int = 1)
                                     (implicit converter: (TSoft, BitCount) => THard): DFGGraph[THard] = {
    require(isPow2(size))
    val dfg = DFGGraph[THard]("fft graph")

    // build the graph layer-by-layer
    dfg

  }

  def main(args: Array[String]): Unit = {

  }
}
