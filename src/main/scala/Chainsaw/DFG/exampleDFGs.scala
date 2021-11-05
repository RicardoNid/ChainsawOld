package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import Chainsaw._
import spinal.core._
import spinal.lib.{Delay, ScalaStream}

import scala.language.postfixOps

object simpleFolding {
  // add some comments
  // val incGen = () => sIntInc(10 bits, 1 cycles).asDSPNode(s"", 1 cycles, 1 ns)
  // val incGen = BinaryNode(sintAdd , "inGen" , 10 bits, 0 cycles , 1 ns)



  def dfg = {
    val initdfg = DFGGraph[SInt]("simpleFolding")
    val incs = initdfg.addConstBinaryNode(4, (i: Int) => i + 1, sintMult, "inc", 10 bits)
    printlnGreen("using simple graph for folding")
    initdfg.setInput(incs(0), 1)
    initdfg.addPath(incs(0) >> 1 >> incs(1) >> 1 >> incs(2) >> 1 >> incs(3))
    initdfg.setOutput(incs(3))
    val foldingSet = Seq(Seq(incs(0), incs(1)), Seq(incs(2), incs(3)))
    (initdfg, foldingSet)
  }


}

object implementingDFGs {
  // FIXME:
  // LQX: this is very confusing, fix it
  def nestedDFG: DFGGraph[SInt] = {

    val butterfly = DFGGraph[SInt]("butterfly")

    val adds: Seq[BinaryNode[SInt]] = Seq.fill(2)(BinaryNode(sintAdd, "add"))
    val mults: Seq[BinaryNode[SInt]] = Seq.fill(2)(BinaryNode(sintMult, "mult"))
    adds.foreach(butterfly.addVertex(_))

    val ins = (0 until 2).map(i => butterfly.addInput(s"in_${i + 1}"))
    butterfly.addEdge(ins(0)(0), adds(0)(0), 0)
    butterfly.addEdge(ins(0)(0), adds(1)(1), 0)
    butterfly.addEdge(ins(1)(0), adds(0)(1), 0)
    butterfly.addEdge(ins(1)(0), adds(1)(0), 0)
    butterfly.setOutput(adds(0))
    butterfly.setOutput(adds(1))

    println(butterfly)

    val whole = DFGGraph[SInt]("simpleNested")
    val butterflies = (0 until 2).map(i => butterfly.asNode[SInt](s"b$i"))

    whole.addVertices(butterflies: _*)
    whole.addEdge(butterflies(0)(0), butterflies(1)(0), 1)
    whole.addEdge(butterflies(0)(1), butterflies(1)(1), 1)
    whole.setInput(butterflies(0), 0)
    whole.setInput(butterflies(0), 1)
    whole.setOutput(butterflies(1), 0)
    whole.setOutput(butterflies(1), 1)
    whole
  }
}

object chap2 {
  def fig2_2 = {
    val Seq(n1, n2, n3, n4, n5, n6) = Seq(1, 1, 1, 2, 2, 2).zipWithIndex.map { case (exe, i) => GeneralNode[SInt](s"node${i + 1}", 0 cycles, exe sec) }
    val dfg = DFGGraph[SInt]("fig2.2")
    dfg.addPath(n1 >> 2 >> n4 >> n2 >> n1)
    dfg.addPath(n1 >> 3 >> n5 >> n3 >> n2)
    dfg.addPath(n1 >> 4 >> n6 >> n3)
    dfg
  }
}

object chap5 {
  val dfg_5_2 = DFGGraph[SInt]("fig5.2")
  val Seq(adds) = dfg_5_2.addBinaryNodes(Seq((sintAdd, 1)), Seq("add"), 10 bits)
  val cmults = dfg_5_2.addConstBinaryNode(1, (i: Int) => 2, sintMult, "cmult", 10 bits)

  def fig5_2 = {
    dfg_5_2.setInput(adds(0))
    dfg_5_2.setOutput(adds(0))
    dfg_5_2.addPath(adds(0) >> 9 >> cmults(0) >> adds(0))
    dfg_5_2
  }

  val dfg_5_2_inner = DFGGraph[SInt]("fig5.2_inner_delay")
  val cmults_inner_delay = dfg_5_2_inner.addConstBinaryNode(1, (i: Int) => 2, sintMult, "cmult", 10 bits)
  val Seq(add_inner_delay) = dfg_5_2_inner.addBinaryNodes(Seq((sintAdd, 1)), Seq("add"), 10 bits, 8 cycles)

  def fig5_2_inner_delay = {
    dfg_5_2_inner.addVertex(add_inner_delay(0))
    dfg_5_2_inner.setInput(add_inner_delay(0))
    dfg_5_2_inner.setOutput(add_inner_delay(0))
    dfg_5_2_inner.addPath(add_inner_delay(0) >> 1 >> cmults_inner_delay(0) >> add_inner_delay(0))
    dfg_5_2_inner
  }

  def fig5_10 = {
    val dfg_5_10 = DFGGraph[SInt]("fig5.10")
    val x = dfg_5_10.addConstBinaryNode(1, (i: Int) => 1, sintMult, "x", 10 bits)
    val Seq(a, b, c) = Seq("a", "b", "c").map(name => dfg_5_10.addConstBinaryNode(1, (i: Int) => 2, sintMult, name, 10 bit))
    val Seq(d, e) = Seq("d", "e").flatMap(name => dfg_5_10.addBinaryNodes(Seq((sintMult, 1)), Seq(name), 10 bit))
    printlnGreen("using fig 5.10")
    dfg_5_10.addPath(x(0) >> c(0) >> 2 >> d(0) >> 4 >> e(0))
    dfg_5_10.addPath(x(0) >> b(0) >> d(0))
    dfg_5_10.addPath(x(0) >> a(0) >> e(0))
    dfg_5_10.setInput(x(0), 1)
    dfg_5_10.setOutput(e(0))
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

  val dfg_6_3 = DFGGraph[SInt]("fig6.3")
  val adds = dfg_6_3.addBinaryNodes(Seq((sintAdd, 4)), Seq("add"), 10 bits).flatten
  // val cmults = (0 until 4).map(i => BinaryNode(sintMult, s"cmult_$i" , 10 bits, 1 cycles, 2 ns))
  val cmults = dfg_6_3.addConstBinaryNode(4, (i: Int) => 2, sintMult, s"cmult", 10 bits, 0 cycles, 2 ns)

  val foldingSet = Seq(
    Seq(adds(3), adds(1), adds(2), adds(0)),
    Seq(cmults(0), cmults(3), cmults(1), cmults(2))
  )

  def fig6_3 = {

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

  // fig6.5(fig 6.3 before retiming)
  def fig6_5 = {
    val dfg = DFGGraph[SInt]("fig6.5")
    (adds ++ cmults).foreach(dfg.addVertex(_))
    val input = dfg.setInput(adds(0))
    val exps = Seq(
      adds(2) >=> 0 >=> adds(0),
      Seq(adds(0), adds(3)) >=> Seq(0, 0) >=> adds(1),
      Seq(cmults(0), cmults(2)) >=> Seq(0, 0) >=> adds(2),
      Seq(cmults(1), cmults(3)) >=> Seq(0, 0) >=> adds(3),
      adds(0) >=> 1 >=> cmults(0),
      adds(0) >=> 1 >=> cmults(1),
      adds(0) >=> 2 >=> cmults(2),
      adds(0) >=> 2 >=> cmults(3))
    exps.foreach(dfg.addExp(_))
    dfg.setOutput(adds(1))
    dfg
  }

  // paper "Synthesis of Control Circuits in Folded Pipelined DSP Architectures"

}

object paper1992OnFolding {

  /*  -----------------------------------fig1_example1------------------------------------*/


  /*  -----------------------------------fig6_a_example3------------------------------------*/


  val dfg_6_a = DFGGraph[SInt]("paper1992fig6_a")
  val cmuls_6_a = dfg_6_a.addConstBinaryNode(4, (i: Int) => 5, sintMult, "cmult", 10 bits)

  def fig6_a = {

    dfg_6_a.addPath(cmuls_6_a(0) >> 0 >> cmuls_6_a(1) >> 1 >> cmuls_6_a(2) >> 2 >> cmuls_6_a(3))
    dfg_6_a.setInput(cmuls_6_a(0), 1)
    dfg_6_a.setOutput(cmuls_6_a(3))
    dfg_6_a
  }

  def foldingSet_example4 = Seq(Seq(cmuls_6_a(0), cmuls_6_a(2)), Seq(cmuls_6_a(1), cmuls_6_a(3)))
  def foldingSet_example3 = Seq(Seq(cmuls_6_a(0), cmuls_6_a(1)), Seq(cmuls_6_a(2), cmuls_6_a(3)))


  /*  -----------------------------------fig6_a_example4------------------------------------*/



  /*  -----------------------------------fig8_a_example6------------------------------------*/

  val dfg_8_a = DFGGraph[SInt]("paper1992fig8_a")
  val cmults_8_a = dfg_8_a.addConstBinaryNode(4, (i: Int) => i + 1, sintMult, "cmult", 10 bits)
  val multadd = TrinaryNode(sintMultAdd, "multadd", 10 bits)

  def fig8_a = {
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

  val dfg_9_a = DFGGraph[SInt]("paper1992fig9_a")
  val cmultadds_9_a = dfg_9_a.addConstTrinaryNode(5, (i: Int) => i % 3 + 1, sintMultAdd, "cmultadd", 10 bits)
  val zero = ConstantNode[SInt, Int]("zero", 0, 10 bits)

  def fig9_a: DFGGraph[SInt] = {
    dfg_9_a.addVertex(zero)
    val x_n = dfg_9_a.addInput(s"x_n")
    dfg_9_a.setOutput(cmultadds_9_a(4), 0, s"y_n")
    cmultadds_9_a.foreach(node => dfg_9_a.addEdge(x_n(0), node(1), 0))
    dfg_9_a.addPath(zero >> cmultadds_9_a(0) >> 1 >> cmultadds_9_a(1) >> 1 >> cmultadds_9_a(2) >> 1 >> cmultadds_9_a(3) >> 1 >> cmultadds_9_a(4))
    dfg_9_a
  }

  def foldingSet9_a_example7 = Seq(
    Seq(cmultadds_9_a(0), cmultadds_9_a(1), cmultadds_9_a(2)),
    Seq(cmultadds_9_a(3), cmultadds_9_a(4), null)
  )

  /*  -----------------------------------fig10_a_example8and10------------------------------------*/
  val dfg_10_a = DFGGraph[SInt]("paper1992fig10")
  val cmult_10_a = dfg_10_a.addConstBinaryNode(4, (i: Int) => i + 1, sintMult, "cmult", 10 bits)

  def fig10 = {
    val x_1 = dfg_10_a.setInput(cmult_10_a(0), 1, s"x1", Seq(Schedule(0, 2)))
    val x_2 = dfg_10_a.setInput(cmult_10_a(0), 1, s"x2", Seq(Schedule(1, 2)))
    dfg_10_a.addPath(cmult_10_a(0) >> 1 >> cmult_10_a(1) >> 1 >> cmult_10_a(2))
    dfg_10_a.addEdge(cmult_10_a(0), cmult_10_a(3), 0, 1, 2, Seq(Schedule(1, 3)))
    dfg_10_a.addEdge(cmult_10_a(2), cmult_10_a(3), 0, 1, 1, Seq(Schedule(0, 3), Schedule(2, 3)))
    dfg_10_a.setOutput(cmult_10_a(3))

    val dfg_10_c = dfg_10_a.clone().asInstanceOf[DFGGraph[SInt]]
    dfg_10_c.setEdgeWeight(dfg_10_c.getEdge(cmult_10_a(0), cmult_10_a(1)), 0)
    dfg_10_c.setEdgeWeight(dfg_10_c.getEdge(cmult_10_a(1), cmult_10_a(2)), 0)
    val cnodes_10_a = cmult_10_a.flatMap(node => dfg_10_c.incomingEdgesOf(node).toArray.map(_.asInstanceOf[DSPEdge[SInt]])
      .filter(dfg_10_c.getEdgeSource(_).isInstanceOf[ConstantNode[SInt]])).map(dfg_10_c.getEdgeSource(_))
    cnodes_10_a.zip(cmult_10_a).foreach { case (cnode, node) => dfg_10_c.setEdgeWeight(dfg_10_c.getEdge(cnode, node), 0) }
    dfg_10_c.removeEdge(dfg_10_c.getEdge(cmult_10_a(0), cmult_10_a(3)))
    dfg_10_c.removeEdge(dfg_10_c.getEdge(cmult_10_a(2), cmult_10_a(3)))
    dfg_10_c.addEdge(cmult_10_a(0), cmult_10_a(3), 0, 1, 0, Seq(Schedule(2, 3)))
    dfg_10_c.addEdge(cmult_10_a(2), cmult_10_a(3), 0, 1, 1, Seq(Schedule(0, 3), Schedule(1, 3)))

    (dfg_10_a, dfg_10_c)
  }

  def fig10_a = fig10._1

  def fig10_c = fig10._2

  def foldingSet10_a_example8and10 = Seq(Seq(cmult_10_a(0), cmult_10_a(1), cmult_10_a(2), cmult_10_a(3)))


  /*  -----------------------------------fig12_a_example11------------------------------------*/

  val dfg_12_a = DFGGraph[SInt]("paper1992fig12_a")
  val cmults_12_a = dfg_12_a.addConstBinaryNode(4, (i: Int) => i + 1, sintMult, "cmult", 10 bits)
  val adds_12_a = dfg_12_a.addBinaryNodes(Seq((sintAdd, 4)), Seq("add"), 10 bits).flatten

  def fig12_a = {
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
  val cmults_13_a = dfg_13_a.addConstBinaryNode(2, (i: Int) => i + 1, sintMult, "cmult", 10 bits)
  val adds_13_a = dfg_13_a.addBinaryNodes(Seq((sintAdd, 2)), Seq("add"), 10 bits).flatten

  def fig13_a = {
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

object printdfg extends App {
  println(paper1992OnFolding.fig13_a)
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