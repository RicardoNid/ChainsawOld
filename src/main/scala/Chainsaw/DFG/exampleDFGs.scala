package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import spinal.core._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object simpleFolding {

  val incs = (0 until 4).map(i => SIntInc.asDSPNode(s"add$i", 1 cycles, 1 ns))
  val Seq(inc0, inc1, inc2, inc3) = incs
  val incGen = () => SIntInc.asDSPNode(s"inc", 1 cycles, 1 ns)
  val foldingSets = Seq(Seq(inc0, inc1), Seq(inc2, inc3))
  val deviceGens = Seq(incGen, incGen)

  def dfg = {
    printlnGreen("using simple graph for folding")
    val dfg = DFG[SInt]
    incs.foreach(dfg.addVertex(_))
    dfg.setInput(inc0)
    dfg.addExp(inc0 >=> 1 >=> inc1)
    dfg.addExp(inc1 >=> 1 >=> inc2)
    dfg.addExp(inc2 >=> 1 >=> inc3)
    dfg.setOutput(inc3)
    dfg
  }
}

object chap6 {

  val adds = (0 until 4).map(i => SIntAdder.asDSPNode(s"add$i", 1 cycles, 1 ns))
  val Seq(adds0, adds1, adds2, adds3) = adds
  val mults = (0 until 4).map(i => SIntCMult.asDSPNode(s"mult$i", 2 cycles, 2 ns))
  val Seq(mults0, mults1, mults2, mults3) = mults

  val addGen = () => SIntAdderPipe.asDSPNode(s"add", 1 cycles, 1 ns)
  val multGen = () => SIntCMultPipe.asDSPNode(s"mult", 2 cycles, 2 ns)

  val deviceGens = Seq(addGen, multGen)
  val foldingSets = Seq(
    Seq(adds3, adds1, adds2, adds0),
    Seq(mults0, mults3, mults1, mults2)
  )

  def dfg6_3 = {
    printlnGreen("using fig 6.3")
    val dfg = DFG[SInt]
    // add vertices
    (adds ++ mults).foreach(dfg.addVertex(_))
    // drive vertices
    val input = dfg.setInput(adds0)
    val exps = Seq(
      adds2 >=> 0 >=> adds0,
      Seq(adds0, adds3) >=> Seq(1, 0) >=> adds1,
      Seq(mults0, mults2) >=> Seq(0, 1) >=> adds2,
      Seq(mults1, mults3) >=> Seq(1, 1) >=> adds3,
      adds0 >=> 1 >=> mults0,
      adds0 >=> 1 >=> mults1,
      adds0 >=> 1 >=> mults2,
      adds0 >=> 2 >=> mults3)
    exps.foreach(dfg.addExp(_))
    dfg.setOutput(adds1)
    dfg
  }

  // fig6.5(fig 6.3 before retiming)
  def fig6_5 = {
    printlnGreen("using fig 6.3 before retiming")
    val dfg = DFG[SInt]
    (adds ++ mults).foreach(dfg.addVertex(_))
    val input = dfg.setInput(adds0)
    val exps = Seq(
      adds2 >=> 0 >=> adds0,
      Seq(adds0, adds3) >=> Seq(0, 0) >=> adds1,
      Seq(mults0, mults2) >=> Seq(0, 0) >=> adds2,
      Seq(mults1, mults3) >=> Seq(0, 0) >=> adds3,
      adds0 >=> 1 >=> mults0,
      adds0 >=> 1 >=> mults1,
      adds0 >=> 2 >=> mults2,
      adds0 >=> 2 >=> mults3)
    exps.foreach(dfg.addExp(_))
    dfg.setOutput(adds1)
    dfg
  }
}

object chap4 {

  def fig4_3 = {
    val Seq(r1, r2, r3, r4) = (0 until 4).map(i => VoidNode[SInt](s"const$i"))
    val cg = ConstraintGraph[SInt]
    cg.addConstraint(r1 - r2 <= 0)
    cg.addConstraint(r3 - r1 <= 5)
    cg.addConstraint(r4 - r1 <= 4)
    cg.addConstraint(r4 - r3 <= -1)
    cg.addConstraint(r3 - r2 <= 2)
    cg
  }

}