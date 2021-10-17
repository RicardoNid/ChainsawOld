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

  val incs = (0 until 4).map(i => SIntCMult(s"cmult_$i", i + 1, 10 bits, 0 cycles, 2 ns))
  val Seq(inc0, inc1, inc2, inc3) = incs
  // add some comments
  val incGen = () => sIntInc(10 bits, 1 cycles).asDSPNode(s"", 1 cycles, 1 ns)
  val foldingSets = Seq(Seq(inc0, inc1), Seq(inc2, inc3))

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
    dfg.addPath(n1 >> 2 >> n4 >> n2 >> n1)
    dfg.addPath(n1 >> 3 >> n5 >> n3 >> n2)
    dfg.addPath(n1 >> 4 >> n6 >> n3)
    dfg
  }
}

object chap5 {
  def fig5_2 = {

    val Seq(a, b, d) = Seq("a", "b", "d").map(name => sintKeep(10 bits).asDSPNode(name, 0 cycles, 1 ns))
    val c = sIntAdder(10 bits, 0 cycles).asDSPNode("c", 0 cycles, 1 ns)
    printlnGreen("using fig 5.2")
    val dfg = DFGGraph[SInt]
    dfg.addPath(a >> c >> 9 >> d >> c)
    dfg.addPath(c >> b)
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
    dfg.addPath(x >> c >> 2 >> d >> 4 >> e)
    dfg.addPath(x >> b >> d)
    dfg.addPath(x >> a >> e)
    dfg.setInput(x)
    dfg.setOutput(e)
    dfg
  }

  def fig5_12 = {
    val zero = SIntConst("sint_0", 0, 10 bits)
    val add = SIntAdderC("add", 10 bits, 0 cycles, 1 ns)

    val dfg = DFGGraph[SInt]
    dfg.addVertex(add)
    dfg.addVertex(zero)
    dfg.setInput(add, 0)
    dfg.setInput(add, 1)
    dfg.setOutput(add, outOrder = 0)

    val zero2add = DefaultDelay[SInt](Seq(Schedule(0, 4)), outOrder = 0, inOrder = 2)
    val out2add = DefaultDelay[SInt](Seq(Schedule(1, 4), Schedule(2, 4), Schedule(3, 4)), outOrder = 1, inOrder = 2)

    dfg.addEdge(add, add,  out2add)
    dfg.setEdgeWeight(out2add, 1)
    dfg.addEdge(zero, add, zero2add)
    dfg.setEdgeWeight(zero2add, 0)

    dfg
  }
}

object chap6 {

  val adds = (0 until 4).map(i => SIntAdder(s"add$i", 10 bits, 0 cycles, 1 ns))
  val Seq(add0, add1, add2, add3) = adds
  val cmults = (0 until 4).map(i => SIntCMult(s"cmult_$i", 2, 10 bits, 1 cycles, 2 ns))
  val Seq(cmult0, cmult1, cmult2, cmult3) = cmults

  val foldingSets = Seq(
    Seq(add3, add1, add2, add0),
    Seq(cmult0, cmult3, cmult1, cmult2)
  )

  def fig6_3 = {
    printlnGreen("using fig 6.3")
    val dfg = DFGGraph[SInt]
    // add vertices
    (adds ++ cmults).foreach(dfg.addVertex(_))
    // drive vertices
    val input = dfg.setInput(add0)
    val exps = Seq(
      add2 >=> 0 >=> add0,
      Seq(add0, add3) >=> Seq(1, 0) >=> add1,
      Seq(cmult0, cmult2) >=> Seq(0, 1) >=> add2,
      Seq(cmult1, cmult3) >=> Seq(1, 1) >=> add3,
      add0 >=> 1 >=> cmult0,
      add0 >=> 1 >=> cmult1,
      add0 >=> 1 >=> cmult2,
      add0 >=> 2 >=> cmult3)
    exps.foreach(dfg.addExp(_))
    dfg.setOutput(add1)
    dfg
  }

  // fig6.5(fig 6.3 before retiming)
  def fig6_5 = {
    printlnGreen("using fig 6.3 before retiming")
    val dfg = DFGGraph[SInt]
    (adds ++ cmults).foreach(dfg.addVertex(_))
    val input = dfg.setInput(add0)
    val exps = Seq(
      add2 >=> 0 >=> add0,
      Seq(add0, add3) >=> Seq(0, 0) >=> add1,
      Seq(cmult0, cmult2) >=> Seq(0, 0) >=> add2,
      Seq(cmult1, cmult3) >=> Seq(0, 0) >=> add3,
      add0 >=> 1 >=> cmult0,
      add0 >=> 1 >=> cmult1,
      add0 >=> 2 >=> cmult2,
      add0 >=> 2 >=> cmult3)
    exps.foreach(dfg.addExp(_))
    dfg.setOutput(add1)
    dfg
  }

  // paper "Synthesis of Control Circuits in Folded Pipelined DSP Architectures"

}

object paper1992OnFolding {

  val cmults = (0 until 5).map(i => SIntCMult(s"cmult_$i", i + 1, 10 bits, 1 cycles, 2 ns))
  val Seq(cmult0, cmult1, cmult2, cmult3, cmult4) = cmults

  def fig8_a = {
    val dfg = DFGGraph[SInt]
    cmults.foreach(dfg.addVertex(_)) // mult0-4 = A1, A2, A3, A4, B
    dfg.addPath(cmult0 >> 1 >> cmult1 >> 1 >> cmult2 >> 2 >> cmult4) // A1 >> A2 >> A3 >> B
    dfg.addPath(cmult0 >> cmult3 >> cmult4) // A1 >> A4 >> B
    dfg.addPath(cmult0 >> cmult4)
    dfg.setInput(cmult0)
    dfg.setOutput(cmult4)
    dfg
  }

  def foldingSet8_a = Seq(Seq(cmult0, cmult1), Seq(cmult3, cmult2), Seq(cmult4, null))
  //  def foldingSet8_a = Seq(Seq(cmult0, cmult1), Seq(cmult2, cmult3), Seq(cmult4, null))
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

object MIMO {

  def fft4 = {

    val butterfly = (dataIns: Seq[ComplexNumber], _: GlobalCount) => {
      val add = dataIns(0) + dataIns(1)
      val sub = dataIns(0) - dataIns(1)
      Seq(add, sub)
    }

    val dfg = DFGGraph[ComplexNumber]

    import Operators._

    val butterflyHardware = DSPHardware(impl = butterfly, inDegree = 2, outWidths = Seq(-1 bits, -1 bits))
    val alphabet = Seq("a", "b", "c", "d", "e")
    val butterflies = Seq.tabulate(2, 2)((i, j) => butterflyHardware.asDSPNode(s"butterfly_${alphabet(i)}${j}", 1 cycles, 1 ns)).flatten
    val Seq(b0, b1, c0, c1) = butterflies
    butterflies.foreach(dfg.addVertex)

    Seq(b0, b1).foreach(butterfly => Seq(0, 1).foreach(dfg.setInput(butterfly, _)))

    // new MIMO API
    dfg.addEdge(b0(0), c0(0), 0)
    dfg.addEdge(b0(1), c1(0), 0)
    dfg.addEdge(b1(0), c0(1), 0)
    dfg.addEdge(b1(1), c1(1), 0)

    Seq(c0, c1).foreach(butterfly => Seq(0, 1).foreach((outOrder: Int) => dfg.setOutput(butterfly, outOrder = outOrder)))

    dfg
  }

}