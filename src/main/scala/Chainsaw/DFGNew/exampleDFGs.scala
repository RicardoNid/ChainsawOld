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

object fig6_3 {

  val adds = (0 until 4).map(i => SIntAdder.asDSPNode(s"add$i", 1 cycles, 1 ns))
  val Seq(adds0, adds1, adds2, adds3) = adds
  val mults = (0 until 4).map(i => SIntMult.asDSPNode(s"mult$i", 2 cycles, 2 ns))
  val Seq(mults0, mults1, mults2, mults3) = mults
  val foldingSet = Seq(
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

  // fig 6.3 before retiming
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
