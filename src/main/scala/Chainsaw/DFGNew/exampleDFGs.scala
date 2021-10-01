package Chainsaw.DFGNew

import Chainsaw.DFGNew.Operators._
import spinal.core._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object simpleFolding {

  val incs = (0 until 4).map(i => SIntInc.asDSPNode(10 bits,s"add$i", 1 cycles, 1 ns))
  val Seq(inc0, inc1,inc2,inc3) = incs
  val incGen = () => SIntInc.asDSPNode(10 bits,s"inc", 1 cycles, 1 ns)
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

object fig6_3 {

  val adds = (0 until 4).map(i => SIntAdder.asDSPNode(10 bits,s"add$i", 1 cycles, 1 ns))
  val Seq(adds0, adds1, adds2, adds3) = adds
  val mults = (0 until 4).map(i => SIntMult.asDSPNode(10 bits,s"mult$i", 2 cycles, 2 ns))
  val Seq(mults0, mults1, mults2, mults3) = mults

  val adderGen = () => SIntAdder.asDSPNode(10 bits,s"add", 1 cycles, 1 ns)
  val multGen = () => SIntMult.asDSPNode(10 bits,s"mult", 2 cycles, 2 ns)
  val deviceGens = Seq(adderGen, multGen)
  val foldingSets = Seq(
    Seq(adds3, adds1, adds2, adds0),
    Seq(mults0, mults3, mults1, mults2)
  )

  def dfg = {
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

  // fig 6.3 before retiming(fig6.5)
  def dfgBeforeRetiming = {
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
