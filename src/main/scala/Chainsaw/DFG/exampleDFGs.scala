package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import Chainsaw._
import spinal.core._

import scala.language.postfixOps

/** the following examples came from:
 *
 * ''VLSI Digital Signal Processing Systems: Design and Implementation'', 1999
 *
 * fig6_3 came from chapter 6, number 3
 *
 * ''Synthesis of Control Circuits in Folded Pipelined DSP Architectures'', 1992
 *
 * fig9_a came from this paper, fig 9 (a)
 */

object simpleFolding {
  // add some comments
  // val incGen = () => sIntInc(10 bits, 1 cycles).asDSPNode(s"", 1 cycles, 1 ns)
  // val incGen = BinaryNode(sintAdd , "inGen" , 10 bits, 0 cycles , 1 ns)

  val incs = (0 until 4).map(i => BinaryNode(sintMult, s"cmult$i", 10 bits, 0 cycles, 2 ns))

  def dfg: DFGGraph[SInt] = {
    printlnGreen("using simple graph for folding")
    implicit val dfg = DFGGraph[SInt]("simpleFolding")

    dfg.addVertices(incs: _*)
    incs.zipWithIndex.foreach { case (inc, i) => inc.addConstantDriver(i + 1, 10 bits, 0) }

    dfg.setInput(incs(0), 1)
    dfg.addPath(incs(0) >> 1 >> incs(1) >> 1 >> incs(2) >> 1 >> incs(3))
    dfg.setOutput(incs(3))
    dfg
  }

  def foldingSet = Seq(Seq(incs(0), incs(1)), Seq(incs(2), incs(3)))


}

object implementingDFGs {

  val adds: Seq[BinaryNode[SInt]] = Seq.tabulate(2)(i => BinaryNode(sintAdd, s"add${i + 1}"))
  val mults: Seq[BinaryNode[SInt]] = Seq.tabulate(2)(i => BinaryNode(sintMult, s"mult${i + 1}"))

  def nestedDFG: DFGGraph[SInt] = {
    val butterfly = DFGGraph[SInt]("butterfly")
    adds.foreach(butterfly.addVertex(_))

    val ins = (0 until 2).map(i => butterfly.addInput(s"in${i + 1}"))
    butterfly.addEdge(ins(0)(0), adds(0)(0), 0)
    butterfly.addEdge(ins(0)(0), adds(1)(1), 0)
    butterfly.addEdge(ins(1)(0), adds(0)(1), 0)
    butterfly.addEdge(ins(1)(0), adds(1)(0), 0)
    butterfly.setOutput(adds(0))
    butterfly.setOutput(adds(1))

    println(butterfly)

    val whole = DFGGraph[SInt]("simpleNested")
    val butterflyNodes = (0 until 2).map(i => butterfly.asNode(s"b${i + 1}"))

    whole.addVertices(butterflyNodes: _*)
    whole.addEdge(butterflyNodes(0)(0), butterflyNodes(1)(0), 1)
    whole.addEdge(butterflyNodes(0)(1), butterflyNodes(1)(1), 1)
    whole.setInput(butterflyNodes(0), 0)
    whole.setInput(butterflyNodes(0), 1)
    whole.setOutput(butterflyNodes(1), 0)
    whole.setOutput(butterflyNodes(1), 1)
    whole
  }
}

object chap2 {
  def fig2_2 = {
    val Seq(n1, n2, n3, n4, n5, n6) = Seq(1, 1, 1, 2, 2, 2).zipWithIndex.map { case (exe, i) => DeviceNode[SInt](s"node${i + 1}", 0 cycles, exe sec) }
    val dfg = DFGGraph[SInt]("fig2.2")

    dfg.addPath(n1 >> 2 >> n4 >> n2 >> n1)
    dfg.addPath(n1 >> 3 >> n5 >> n3 >> n2)
    dfg.addPath(n1 >> 4 >> n6 >> n3)
    dfg
  }
}

object chap5 {

  val add = BinaryNode(sintAdd, "add", 10 bits, 0 cycles, 1 ns)
  val add_inner_delay = BinaryNode(sintAdd, "add0", 10 bits, 8 cycles, 1 ns)
  val cmult = BinaryNode(sintMult, "cmult", 10 bits, 0 cycles, 1 ns)

  def fig5_2 = {
    val dfg_5_2 = DFGGraph[SInt]("fig5.2")

    dfg_5_2.addVertex(add)
    dfg_5_2.genConstBinaryNode(cmult, 2)
    dfg_5_2.setInput(add)
    dfg_5_2.setOutput(add)
    dfg_5_2.addPath(add >> 9 >> cmult >> add)
    dfg_5_2
  }

  def fig5_2_inner_delay = {
    val dfg_5_2_inner = DFGGraph[SInt]("fig5.2_inner_delay")

    dfg_5_2_inner.addVertex(add_inner_delay)
    dfg_5_2_inner.setInput(add_inner_delay)
    dfg_5_2_inner.setOutput(add_inner_delay)
    dfg_5_2_inner.addPath(add_inner_delay >> 1 >> cmult >> add_inner_delay)
    dfg_5_2_inner
  }

  def fig5_10 = {
    val dfg_5_10 = DFGGraph[SInt]("fig5.10")

    val x = BinaryNode(sintMult, "x", 10 bits, 0 cycles, 1 ns)
    dfg_5_10.genConstBinaryNode(x, 1)
    val Seq(a, b, c) = Seq("a", "b", "c").map(name => BinaryNode(sintMult, name, 10 bits, 0 cycles, 1 ns))
    Seq(a, b, c).foreach(dfg_5_10.genConstBinaryNode(_, 2))
    val Seq(d, e) = Seq("d", "e").map(name => BinaryNode(sintAdd, name, 10 bits, 0 cycles, 1 ns))
    printlnGreen("using fig 5.10")
    dfg_5_10.addPath(x >> c >> 2 >> d >> 4 >> e)
    dfg_5_10.addPath(x >> b >> d)
    dfg_5_10.addPath(x >> a >> e)
    dfg_5_10.setInput(x, 1)
    dfg_5_10.setOutput(e)
    dfg_5_10
  }

  def fig5_12 = {
    val zero = ConstantNode[SInt, Int]("zero", 0, 10 bits)
    val add = AdderC(sintAddC, "add", Seq(10 bits, 1 bits))

    val dfg = DFGGraph[SInt]("fig5.12")
    dfg.addVertex(add)
    dfg.addVertex(zero)
    dfg.setInput(add, 0)
    dfg.setInput(add, 1)
    dfg.setOutput(add, outOrder = 0)
    dfg.addEdge(zero(0), add(2), 0, Seq(Schedule(0, 4)))
    dfg.addEdge(add(1), add(2), 1, Seq(Schedule(1, 4), Schedule(2, 4), Schedule(3, 4)))

    dfg
  }
}

object chap6 {

  val adds = (0 until 4).map(i => BinaryNode(sintAdd, s"add$i", 10 bits, 0 cycles, 1 ns))
  // val cmults = (0 until 4).map(i => BinaryNode(sintMult, s"cmult_$i" , 10 bits, 1 cycles, 2 ns))
  val cmults = (0 until 4).map(i => BinaryNode(sintMult, s"cmult$i", 10 bits, 0 cycles, 2 ns))

  def fig6_3 = {
    val dfg_6_3 = DFGGraph[SInt]("fig6.3")

    adds.foreach(dfg_6_3.addVertex(_))
    cmults.foreach(dfg_6_3.genConstBinaryNode(_, 2))
    // drive vertices
    val input = dfg_6_3.setInput(adds(0))
    val exps = Seq(
      adds(2) >=> 0 >=> adds(0),
      Seq(adds(0), adds(3)) >=> Seq(1, 0) >=> adds(1),
      Seq(cmults(0), cmults(2)) >=> Seq(0, 1) >=> adds(2),
      Seq(cmults(1), cmults(3)) >=> Seq(1, 1) >=> adds(3),
      adds(0) >=> 1 >=> cmults(0),
      adds(0) >=> 1 >=> cmults(1),
      adds(0) >=> 1 >=> cmults(2),
      adds(0) >=> 2 >=> cmults(3))
    exps.foreach(dfg_6_3.addExp(_))
    dfg_6_3.setOutput(adds(1))
    dfg_6_3
  }

  def foldingSet = Seq(
    Seq(adds(3), adds(1), adds(2), adds(0)),
    Seq(cmults(0), cmults(3), cmults(1), cmults(2))
  )

  // fig6.5(fig 6.3 before retiming)
  def fig6_5 = {
    val dfg_6_5 = DFGGraph[SInt]("fig6.5")
    adds.foreach(dfg_6_5.addVertex(_))
    cmults.foreach(dfg_6_5.genConstBinaryNode(_, 2))
    val input = dfg_6_5.setInput(adds(0))
    val exps = Seq(
      adds(2) >=> 0 >=> adds(0),
      Seq(adds(0), adds(3)) >=> Seq(0, 0) >=> adds(1),
      Seq(cmults(0), cmults(2)) >=> Seq(0, 0) >=> adds(2),
      Seq(cmults(1), cmults(3)) >=> Seq(0, 0) >=> adds(3),
      adds(0) >=> 1 >=> cmults(0),
      adds(0) >=> 1 >=> cmults(1),
      adds(0) >=> 2 >=> cmults(2),
      adds(0) >=> 2 >=> cmults(3))
    exps.foreach(dfg_6_5.addExp(_))
    dfg_6_5.setOutput(adds(1))
    dfg_6_5
  }

  // paper "Synthesis of Control Circuits in Folded Pipelined DSP Architectures"

}

object paper1992OnFolding {

  /*  -----------------------------------fig1_example1------------------------------------*/


  /*  -----------------------------------fig6_a_example3------------------------------------*/

  val cmults = (0 until 4).map(i => BinaryNode(sintMult, s"cmult$i", 10 bits))

  def fig6_a = {
    val dfg_6_a = DFGGraph[SInt]("paper1992fig6_a")
    cmults.foreach(dfg_6_a.genConstBinaryNode(_, 5))
    dfg_6_a.addPath(cmults(0) >> 0 >> cmults(1) >> 1 >> cmults(2) >> 2 >> cmults(3))
    dfg_6_a.setInput(cmults(0), 1)
    dfg_6_a.setOutput(cmults(3))
    dfg_6_a
  }

  def foldingSet_example4 = Seq(Seq(cmults(0), cmults(2)), Seq(cmults(1), cmults(3)))

  def foldingSet_example3 = Seq(Seq(cmults(0), cmults(1)), Seq(cmults(2), cmults(3)))


  /*  -----------------------------------fig6_a_example4------------------------------------*/


  /*  -----------------------------------fig8_a_example6------------------------------------*/

  val multadd = TrinaryNode(sintMultAdd, "multadd", 10 bits)
  val cmults_8_a = (0 until 4).map(i => BinaryNode(sintMult, s"cmult${i + 1}", 10 bits, 0 cycles, 1 ns))

  def fig8_a = {
    val dfg_8_a = DFGGraph[SInt]("paper1992fig8_a")
    cmults_8_a.zipWithIndex.foreach { case (cm, id) => dfg_8_a.genConstBinaryNode(cm, id + 1) }
    dfg_8_a.genConstTrinaryNode(multadd, 5)
    dfg_8_a.addPath(cmults_8_a(0) >> 1 >> cmults_8_a(1) >> 1 >> cmults_8_a(2) >> 2 >> multadd) // A1 >> A2 >> A3 >> B
    dfg_8_a.addPath(cmults_8_a(0) >> cmults_8_a(3) >> multadd) // A1 >> A4 >> B
    dfg_8_a.addPath(cmults_8_a(0) >> multadd) // A1 >> B
    dfg_8_a.setInput(cmults_8_a(0), 1)
    dfg_8_a.setOutput(multadd)
    dfg_8_a
  }

  def foldingSet8_a_example6 = Seq(Seq(cmults_8_a(0), cmults_8_a(1)), Seq(cmults_8_a(3), cmults_8_a(2)), Seq(multadd, null))
  //  def foldingSet8_a = Seq(Seq(cmult0, cmult1), Seq(cmult2, cmult3), Seq(cmult4, null))

  /*  -----------------------------------fig9_a_example7------------------------------------*/

  val cmultadds = (0 until 5).map(i => TrinaryNode(sintMultAdd, s"cmultadd${i + 1}", 10 bits, 0 cycles, 1 ns))
  val zero = ConstantNode[SInt, Int]("zero", 0, 10 bits)

  def fig9_a: DFGGraph[SInt] = {
    val dfg_9_a = DFGGraph[SInt]("paper1992fig9_a")
    dfg_9_a.addVertex(zero)
    cmultadds.zipWithIndex.foreach { case (cma, id) => dfg_9_a.genConstTrinaryNode(cma, id % 3 + 1) }
    val x_n = dfg_9_a.addInput(s"x_n")
    dfg_9_a.setOutput(cmultadds(4), 0, s"y_n")
    cmultadds.foreach(node => dfg_9_a.addEdge(x_n(0), node(1), 0))
    dfg_9_a.addPath(zero >> cmultadds(0) >> 1 >> cmultadds(1) >> 1 >> cmultadds(2) >> 1 >> cmultadds(3) >> 1 >> cmultadds(4))
    dfg_9_a
  }

  def foldingSet9_a_example7 = Seq(
    Seq(cmultadds(0), cmultadds(1), cmultadds(2)),
    Seq(cmultadds(3), cmultadds(4), null)
  )

  /*  -----------------------------------fig10_a_example8and10------------------------------------*/

  val cmults_10_a = (0 until 4).map(i => BinaryNode(sintMult, s"A${i + 1}", 10 bits, 0 cycles, 1 ns))

  def fig10 = {
    val dfg_10_a = DFGGraph[SInt]("paper1992fig10")

    cmults_10_a.zipWithIndex.foreach { case (cm, id) => dfg_10_a.genConstBinaryNode(cm, id + 1) }
    val x_1 = dfg_10_a.setInput(cmults_10_a(0), 1, s"x1", Seq(Schedule(0, 2)))
    val x_2 = dfg_10_a.setInput(cmults_10_a(0), 1, s"x2", Seq(Schedule(1, 2)))
    dfg_10_a.addPath(cmults_10_a(0) >> 1 >> cmults_10_a(1) >> 1 >> cmults_10_a(2))
    dfg_10_a.addEdge(cmults_10_a(0), cmults_10_a(3), 0, 1, 2, Seq(Schedule(1, 3)))
    dfg_10_a.addEdge(cmults_10_a(2), cmults_10_a(3), 0, 1, 1, Seq(Schedule(0, 3), Schedule(2, 3)))
    dfg_10_a.setOutput(cmults_10_a(3))

    val dfg_10_c = dfg_10_a.clone().asInstanceOf[DFGGraph[SInt]]
    dfg_10_c.setEdgeWeight(dfg_10_c.getEdge(cmults_10_a(0), cmults_10_a(1)), 0)
    dfg_10_c.setEdgeWeight(dfg_10_c.getEdge(cmults_10_a(1), cmults_10_a(2)), 0)
    val cnodes_10_a = cmults_10_a.flatMap(node => dfg_10_c.incomingEdgesOf(node).toArray.map(_.asInstanceOf[DSPEdge[SInt]])
      .filter(dfg_10_c.getEdgeSource(_).isInstanceOf[ConstantNode[SInt]])).map(dfg_10_c.getEdgeSource(_))
    cnodes_10_a.zip(cmults_10_a).foreach { case (cnode, node) => dfg_10_c.setEdgeWeight(dfg_10_c.getEdge(cnode, node), 0) }
    dfg_10_c.removeEdge(dfg_10_c.getEdge(cmults_10_a(0), cmults_10_a(3)))
    dfg_10_c.removeEdge(dfg_10_c.getEdge(cmults_10_a(2), cmults_10_a(3)))
    dfg_10_c.addEdge(cmults_10_a(0), cmults_10_a(3), 0, 1, 0, Seq(Schedule(2, 3)))
    dfg_10_c.addEdge(cmults_10_a(2), cmults_10_a(3), 0, 1, 1, Seq(Schedule(0, 3), Schedule(1, 3)))

    (dfg_10_a, dfg_10_c)
  }

  def fig10_a = fig10._1

  def fig10_c = fig10._2

  def foldingSet10_a_example8and10 = Seq(Seq(cmults_10_a(0), cmults_10_a(1), cmults_10_a(2), cmults_10_a(3)))


  /*  -----------------------------------fig12_a_example11------------------------------------*/

  val cmults_12_a = (0 until 4).map(i => BinaryNode(sintMult, s"M${i + 1}", 10 bits, 0 cycles, 1 ns))
  val adds_12_a = (0 until 4).map(i => BinaryNode(sintAdd, s"A${i + 1}", 10 bits, 0 cycles, 1 ns))

  def fig12_a = {
    val dfg_12_a = DFGGraph[SInt]("paper1992fig12_a")
    adds_12_a.foreach(dfg_12_a.addVertex(_))
    cmults_12_a.zipWithIndex.foreach { case (cm, id) => dfg_12_a.genConstBinaryNode(cm, id + 1) }
    dfg_12_a.setInput(adds_12_a(0))
    dfg_12_a.addPath(adds_12_a(0) >> adds_12_a(1))
    dfg_12_a.setOutput(adds_12_a(1))
    dfg_12_a.addPath(adds_12_a(0) >> 1 >> cmults_12_a(0) >> adds_12_a(2) >> adds_12_a(0))
    dfg_12_a.addPath(adds_12_a(0) >> 1 >> cmults_12_a(1) >> adds_12_a(3) >> adds_12_a(1))
    dfg_12_a.addPath(adds_12_a(0) >> 2 >> cmults_12_a(2) >> adds_12_a(2))
    dfg_12_a.addPath(adds_12_a(0) >> 2 >> cmults_12_a(3) >> adds_12_a(3))
    dfg_12_a
  }

  def foldingSet_example11 = Seq(Seq(adds_12_a(3), adds_12_a(1), null, adds_12_a(2), adds_12_a(0)), Seq(cmults_12_a(0), cmults_12_a(1), cmults_12_a(2), cmults_12_a(3), null))

  /*  -----------------------------------fig13_a_example12------------------------------------*/

  val dfg_13_a = DFGGraph[SInt]("paper1992fig13_a")
  val cmults_13_a = (0 until 2).map(i => BinaryNode(sintMult, s"cmult_$i", 10 bits, 0 cycles, 1 ns))
  val adds_13_a = (0 until 2).map(i => BinaryNode(sintAdd, s"cadder_$i", 10 bits, 0 cycles, 1 ns))

  def fig13_a = {
    val dfg_13_a = DFGGraph[SInt]("paper1992fig13_a")
    adds_13_a.foreach(dfg_13_a.addVertex(_))
    cmults_13_a.zipWithIndex.foreach { case (cm, id) => dfg_13_a.genConstBinaryNode(cm, id + 1) }
    dfg_13_a.addPath(adds_13_a(0) >> 1 >> cmults_13_a(0) >> 1 >> adds_13_a(1) >> 1 >> cmults_13_a(1) >> 2 >> adds_13_a(0))
    dfg_13_a.setInput(adds_13_a(0), 1, s"input0")
    dfg_13_a.setInput(adds_13_a(1), 1, s"input1")
    dfg_13_a.setOutput(cmults_13_a(1), 0, s"output0")
    dfg_13_a
  }

  def foldingSet13_a_example12 = Seq(Seq(adds_13_a(0), adds_13_a(1)), Seq(cmults_13_a(0), cmults_13_a(1)))

  /*  -----------------------------------fig14_a_example13------------------------------------*/

  val adderc_fig14_a = (0 until 3).map(i => AdderC(sintAddC, s"A${i + 1}", Seq(10 bits, 1 bits), 0 cycles, 1 ns))

  def fig14_a: DFGGraph[SInt] = {
    val dfg = DFGGraph[SInt]("paper1992fig14_a")
    adderc_fig14_a.foreach(dfg.addVertex(_))
    dfg.addVertex(zero)
    // zero => adder
    adderc_fig14_a.zipWithIndex.foreach { case (ac, i) => dfg.addEdge(zero(0), ac(2), 0, Seq(Schedule(i, 4))) }
    // carry => next adder
    dfg.addEdge(adderc_fig14_a(0)(1), adderc_fig14_a(1)(2), 0, Seq(Schedule(0, 4), Schedule(2, 4), Schedule(3, 4)))
    dfg.addEdge(adderc_fig14_a(1)(1), adderc_fig14_a(2)(2), 0, Seq(Schedule(0, 4), Schedule(1, 4), Schedule(3, 4)))
    dfg.addEdge(adderc_fig14_a(2)(1), adderc_fig14_a(0)(2), 1, Seq(Schedule(1, 4), Schedule(2, 4), Schedule(3, 4)))
    // input => adder
    adderc_fig14_a.zipWithIndex.foreach { case (ac, i) => dfg.setInput(ac, 0, s"x_3k_$i") }
    adderc_fig14_a.zipWithIndex.foreach { case (ac, i) => dfg.setInput(ac, 1, s"y_3k_$i") }
    adderc_fig14_a.zipWithIndex.foreach { case (ac, i) => dfg.setOutput(ac, 0, s"s_3k_$i") }

    dfg
  }

  def foldingSet_example13 = Seq(Seq(adderc_fig14_a(0), adderc_fig14_a(1), adderc_fig14_a(2)))

  def foldingSet14_a_example13_v2 = Seq(Seq(adderc_fig14_a(1), adderc_fig14_a(2), adderc_fig14_a(0)))
}


object chap4 {
  def fig4_3 = {
    val Seq(r1, r2, r3, r4) = (0 until 4).map(i => VirtualNode[SInt](s"const$i"))
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

    val dfg = DFGGraph[ComplexNumber]("fft4")

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