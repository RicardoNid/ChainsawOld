package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import Chainsaw._
import spinal.core._

object ShowGraphs {
  def main(args: Array[String]): Unit = {
    println(chap2.fig2_2)
  }
}

object simpleFolding {

  val incs = (0 until 4).map(i => sIntInc(10 bits, 0 cycles).asDSPNode(s"increase$i", 0 cycles, 1 ns))
  val Seq(inc0, inc1, inc2, inc3) = incs
  val incGen = () => sIntInc(10 bits, 1 cycles).asDSPNode(s"", 1 cycles, 1 ns)
  val foldingSets = Seq(Seq(inc0, inc1), Seq(inc2, inc3))
  val deviceGens = Seq(incGen, incGen)

  def dfg = {
    printlnGreen("using simple graph for folding")
    val dfg = DFGGraph[SInt]
    incs.foreach(dfg.addVertex(_))
    dfg.setInput(inc0)
    dfg.addExp(inc0 >=> 1 >=> inc1)
    dfg.addExp(inc1 >=> 1 >=> inc2)
    dfg.addExp(inc2 >=> 1 >=> inc3)
    dfg.setOutput(inc3)
    dfg
  }
}

object chap2 {
  def fig2_2 = {
    val Seq(n1, n2, n3, n4, n5, n6) = Seq(1, 1, 1, 2, 2, 2).zipWithIndex.map { case (exe, i) => GeneralNode[SInt](s"node${i + 1}", 0 cycles, exe sec) }
    printlnGreen("using fig 2.2")
    val dfg = DFGGraph[SInt]
    dfg.addPath(n1 >> 2 >> n4 >> 0 >> n2 >> 0 >> n1)
    dfg.addPath(n1 >> 3 >> n5 >> 0 >> n3 >> 0 >> n2)
    dfg.addPath(n1 >> 4 >> n6 >> 0 >> n3)
    dfg
  }
}

object chap5 {
  def fig5_2 = {
    val Seq(a, b, d) = Seq("a", "b", "d").map(name => sintKeep(10 bits).asDSPNode(name, 0 cycles, 1 ns))
    val c = sIntAdder(10 bits, 0 cycles).asDSPNode("c", 0 cycles, 1 ns)
    printlnGreen("using fig 5.2")
    val dfg = DFGGraph[SInt]
    dfg.addPath(a >> 0 >> c >> 9 >> d >> 0 >> c)
    dfg.addPath(c >> 0 >> b)
    dfg.setInput(a)
    dfg.setOutput(b)
    dfg
  }

  def fig5_10 = {
    val x = sintKeep.asDSPNode("x", 0 cycles, 1 ns)
    val Seq(a, b, c) = Seq("a", "b", "c").map(name => sIntCMult(2, 10 bits, 0 cycles).asDSPNode(name, 0 cycles, 1 ns))
    val Seq(d, e) = Seq("d", "e").map(name => sIntAdder(10 bits, 0 cycles).asDSPNode(name, 0 cycles, 1 ns))
    printlnGreen("using fig 5.10")
    val dfg = DFGGraph[SInt]
    dfg.addPath(x >> 0 >> c >> 2 >> d >> 4 >> e)
    dfg.addPath(x >> 0 >> b >> 0 >> d)
    dfg.addPath(x >> 0 >> a >> 0 >> e)
    dfg.setInput(x)
    dfg.setOutput(e)
    dfg
  }
}

object chap6 {
  val adds = (0 until 4).map(i => sIntAdder(10 bits, 0 cycles).asDSPNode(s"add$i", 1 cycles, 1 ns))
  val Seq(adds0, adds1, adds2, adds3) = adds
  val mults = (0 until 4).map(i => sIntCMult(i, 10 bits, 0 cycles).asDSPNode(s"mult$i", 2 cycles, 2 ns))
  //  val mults = (0 until 4).map(i => sIntCMult(1, 10 bits, 0 cycles).asDSPNode(s"mult$i", 2 cycles, 2 ns))
  val Seq(mults0, mults1, mults2, mults3) = mults

  val addGen = () => sIntAdder(10 bits, 1 cycles).asDSPNode(s"add", 1 cycles, 1 ns)
  val multGen = () => sIntCMultFolded(Seq(0, 3, 1, 2), 10 bits, 2 cycles).asDSPNode(s"mult", 2 cycles, 2 ns)
  //  val multGen = () => sIntCMult(1, 10 bits, 2 cycles).asDSPNode(s"mult", 2 cycles, 2 ns)

  val deviceGens = Seq(addGen, multGen)
  val foldingSets = Seq(
    Seq(adds3, adds1, adds2, adds0),
    Seq(mults0, mults3, mults1, mults2)
  )

  def fig6_3 = {
    printlnGreen("using fig 6.3")
    val dfg = DFGGraph[SInt]
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
    val dfg = DFGGraph[SInt]
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

object fft {

}