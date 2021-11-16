package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import Chainsaw._
import spinal.core._

import scala.collection.immutable
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

  val incs = (0 until 4).map(i => BinaryHardware(sintMult, 10 bits, 0 cycles, 2 ns).asDeviceNode(s"cmult$i"))

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

  val adds: Seq[BinaryNode[SInt]] = Seq.tabulate(2)(i => BinaryHardware(sintAdd).asDeviceNode(s"add${i + 1}"))
  val mults: Seq[BinaryNode[SInt]] = Seq.tabulate(2)(i => BinaryHardware(sintMult).asDeviceNode(s"mult${i + 1}"))

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
  println(butterfly.ioPositions.mkString(" "))
  val butterflyNode = butterfly.asNode("butterflyNode")

  def nestedDFG: DFGGraph[SInt] = {
    val whole = DFGGraph[SInt]("simpleNested")
    val butterflyNodes = (0 until 2).map(i => butterflyNode.copy(s"b${i + 1}"))
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
  // FIXME: this example should use exeTime, so we should provide a corresponding node interface on this
  def fig2_2 = {
    val Seq(n1, n2, n3, n4, n5, n6) = Seq(1, 1, 1, 2, 2, 2).zipWithIndex.map { case (exe, i) => DeviceNode[SInt](s"node${i + 1}", Operators.passThrough()) }
    val dfg = DFGGraph[SInt]("fig2.2")

    dfg.addPath(n1 >> 2 >> n4 >> n2 >> n1)
    dfg.addPath(n1 >> 3 >> n5 >> n3 >> n2)
    dfg.addPath(n1 >> 4 >> n6 >> n3)
    dfg
  }
}

object chap5 {

  val add = BinaryHardware(sintAdd, 10 bits, 0 cycles, 1 ns).asDeviceNode("add")
  val add_inner_delay = BinaryHardware(sintAdd, 10 bits, 8 cycles, 1 ns).asDeviceNode("add0")
  val cmult = BinaryHardware(sintMult, 10 bits, 0 cycles, 1 ns).asDeviceNode("cmult")

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

    val x = BinaryHardware(sintMult, 10 bits, 0 cycles, 1 ns).asDeviceNode("x")
    dfg_5_10.genConstBinaryNode(x, 1)
    val Seq(a, b, c) = Seq("a", "b", "c").map(name => BinaryHardware(sintMult, 10 bits, 0 cycles, 1 ns).asDeviceNode(name))
    Seq(a, b, c).foreach(dfg_5_10.genConstBinaryNode(_, 2))
    val Seq(d, e) = Seq("d", "e").map(name => BinaryHardware(sintAdd, 10 bits, 0 cycles, 1 ns).asDeviceNode(name))
    dfg_5_10.addPath(x >> c >> 2 >> d >> 4 >> e)
    dfg_5_10.addPath(x >> b >> d)
    dfg_5_10.addPath(x >> a >> e)
    dfg_5_10.setInput(x, 1)
    dfg_5_10.setOutput(e)
    dfg_5_10
  }

  def fig5_12 = {
    val zero = ConstantNode[SInt, Int]("zero", 0, 10 bits)
    val add = sintAddCNode(10 bits)

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

  val adds = (0 until 4).map(i => BinaryHardware(sintAdd, 10 bits, 0 cycles, 1 ns).asDeviceNode(s"add$i"))
  // val cmults = (0 until 4).map(i => BinaryNode(sintMult, s"cmult_$i" , 10 bits, 1 cycles, 2 ns))
  val cmults = (0 until 4).map(i => BinaryHardware(sintMult, 10 bits, 0 cycles, 2 ns).asDeviceNode(s"cmult$i"))

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
  def fig6_5: DFGGraph[SInt] = {
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

  val unary: UnaryHardware[SInt] = UnaryHardware(Operators.sintInc, 10 bits)
  val binary0: BinaryHardware[SInt] = BinaryHardware(Operators.sintAdd, 10 bits)
  val binary1: BinaryHardware[SInt] = BinaryHardware(Operators.sintMult, 10 bits)
  val trinary: TrinaryHardware[SInt] = TrinaryHardware(Operators.sintMultAdd, 10 bits)

  // nodes tobe used
  val as: Seq[DeviceNode[SInt]] = unary.asDeviceNodes("A", 4)
  val Seq(a1, a2, a3, a4) = as

  val b: TrinaryNode[SInt] = trinary.asDeviceNode("B")

  val mas: Seq[DeviceNode[SInt]] = trinary.asDeviceNodes("MA", 5)
  val Seq(ma1, ma2, ma3, ma4, ma5) = mas

  val adds: Seq[DeviceNode[SInt]] = binary0.asDeviceNodes("A", 4)
  val Seq(add1, add2, add3, add4) = adds

  val mults: Seq[DeviceNode[SInt]] = binary1.asDeviceNodes("M", 4)
  val Seq(mult1, mult2, mult3, mult4) = mults

  val addcs: Seq[DSPNode[SInt]] = (1 to 3).map(i => sintAddCNode(10 bits).copy(s"A_$i"))
  val Seq(addc1, addc2, addc3) = addcs

  lazy val fig6_a: DFGGraph[SInt] = {
    val dfg_6_a = DFGGraph[SInt]("paper1992fig6_a")
    as.foreach(dfg_6_a.addVertex(_))
    dfg_6_a.addPath(a1 >> 0 >> a2 >> 1 >> a3 >> 2 >> a4)
    dfg_6_a.setInput(a1)
    dfg_6_a.setOutput(a4)
    dfg_6_a
  }

  lazy val foldingSet_example3 = Seq(Seq(a1, a2), Seq(a3, a4))
  lazy val foldingSet_example4 = Seq(Seq(a1, a3), Seq(a2, a4))

  lazy val fig8_a: DFGGraph[SInt] = {
    val dfg_8_a = DFGGraph[SInt]("paper1992fig8_a")
    as.foreach(dfg_8_a.addVertex(_))
    dfg_8_a.addPath(a1 >> 1 >> a2 >> 1 >> a3 >> 2 >> b) // A1 >> A2 >> A3 >> B
    dfg_8_a.addPath(a1 >> a4 >> b)
    dfg_8_a.addPath(a1 >> b)
    dfg_8_a.setInput(a1)
    dfg_8_a.setOutput(b)
    dfg_8_a
  }

  lazy val foldingSet_example6 = Seq(Seq(a1, a2), Seq(a4, a3), Seq(b, null))

  lazy val fig9_a: DFGGraph[SInt] = {
    implicit val dfg_9_a: DFGGraph[SInt] = DFGGraph[SInt]("paper1992fig9_a")
    mas.foreach(dfg_9_a.addVertex(_))
    val xn = dfg_9_a.addInput(s"xn")
    mas.foreach(node => dfg_9_a.addEdge(xn, node, 0)) // port0
    mas.zipWithIndex.foreach { case (ma, i) => ma.addConstantDriver(i + 1, 10 bits, 1) } // port1
    dfg_9_a.addPath(ma1 >> 1 >> ma2 >> 1 >> ma3 >> 1 >> ma4 >> 1 >> ma5) // port2
    ma1.addConstantDriver(0, 10 bits, 2) // port2
    dfg_9_a.setOutput(ma5, 0, s"yn")
    dfg_9_a
  }

  lazy val foldingSet_example7 = Seq(Seq(ma1, ma2, ma3), Seq(ma4, ma5, null))

  // following examples have periods > 1 originally

  lazy val fig10_a: DFGGraph[SInt] = {
    val dfg_10_a = DFGGraph[SInt]("paper1992fig10_a")
    as.foreach(dfg_10_a.addVertex(_))
    dfg_10_a.addPath(a1 >> 1 >> a2 >> 1 >> a3)
    dfg_10_a.addEdge(a1, a4, 2, Seq(Schedule(1, 3)))
    dfg_10_a.addEdge(a3, a4, 1, Seq(Schedule(0, 3), Schedule(2, 3)))
    val x1 = dfg_10_a.setInput(a1, 0, s"x1", Seq(Schedule(0, 2)))
    val x2 = dfg_10_a.setInput(a1, 0, s"x2", Seq(Schedule(1, 2)))
    dfg_10_a.setOutput(a4)
    dfg_10_a
  }

  lazy val fig10_c: DFGGraph[SInt] = {
    val dfg_10_c = DFGGraph[SInt]("paper1992fig10_c")
    as.foreach(dfg_10_c.addVertex(_))
    dfg_10_c.addPath(a1 >> a2 >> a3)
    dfg_10_c.addEdge(a1, a4, 0, Seq(Schedule(1, 3)))
    dfg_10_c.addEdge(a3, a4, 1, Seq(Schedule(0, 3), Schedule(2, 3)))
    val x1 = dfg_10_c.setInput(a1, 0, s"x1", Seq(Schedule(0, 2)))
    val x2 = dfg_10_c.setInput(a1, 0, s"x2", Seq(Schedule(1, 2)))
    dfg_10_c.setOutput(a4)
    dfg_10_c
  }

  lazy val foldingSet_example8and10 = Seq(Seq(a1, a2), Seq(a3, a4))

  lazy val fig12_a: DFGGraph[SInt] = {
    implicit val dfg_12_a: DFGGraph[SInt] = DFGGraph[SInt]("paper1992fig12_a")
    (adds ++ mults).foreach(dfg_12_a.addVertex(_))
    dfg_12_a.setInput(add1, 0)
    dfg_12_a.setOutput(add2, 0)
    // constants
    mults.zipWithIndex.foreach { case (node, i) => node.addConstantDriver(i + 1, 10 bits, 0) } // mult port 0
    // top
    dfg_12_a.addPath(add1 >> add2)
    // left
    dfg_12_a.addPath(add1 >> 1 >> mult1 >> add3 >> add1)
    dfg_12_a.addPath(add1 >> 2 >> mult3 >> add3)
    // right
    dfg_12_a.addPath(add1 >> 1 >> mult2 >> add4 >> add2)
    dfg_12_a.addPath(add1 >> 2 >> mult4 >> add4)
    dfg_12_a
  }

  lazy val foldingSet_example11 = Seq(Seq(add4, add2, null, add3, add1), Seq(mult1, mult3, mult2, mult4, null))

  lazy val fig13_a: DFGGraph[SInt] = {
    val dfg_13_a = DFGGraph[SInt]("paper1992fig13_a")
    as.foreach(dfg_13_a.addVertex(_))
    val (m1, m2) = (a3, a4)
    dfg_13_a.addPath(a1 >> m1 >> 1 >> a2 >> 2 >> m2 >> 2 >> a1)
    dfg_13_a
  }

  lazy val foldingSet_example12 = Seq(Seq(a1, a2), Seq(a3, a4))


  lazy val fig14_a: DFGGraph[SInt] = {
    implicit val dfg14_a: DFGGraph[SInt] = DFGGraph[SInt]("paper1992fig14_a")
    addcs.foreach(dfg14_a.addVertex(_))
    val xs: Seq[InputNode[SInt]] = addcs.zipWithIndex.map { case (node, i) => dfg14_a.setInput(node, 0, s"x3k_$i") } // in port0
    val ys: Seq[InputNode[SInt]] = addcs.zipWithIndex.map { case (node, i) => dfg14_a.setInput(node, 1, s"y3k_$i") } // in port1
    addcs.zipWithIndex.foreach { case (node, i) => node.addConstantDriver(0, 1 bits, 2, Seq(Schedule(i, 4))) } // in port2

    def except(i: Int): Seq[Schedule] = (0 until 4).filterNot(_ == i).map(j => Schedule(j, 4))

    addcs.zip(addcs.tail :+ addcs.head).zip(Seq(0, 0, 1).zipWithIndex).foreach { case ((prev, next), (delay, i)) =>
      dfg14_a.addEdge(prev(1), next(2), delay, except((i + 1) % 3))
    } // out port1 -> in port2
    val ss: Seq[OutputNode[SInt]] = addcs.zipWithIndex.map { case (node, i) => dfg14_a.setOutput(node, 0, s"s3k_$i") } // out port0
    dfg14_a
  }

  lazy val foldingSet_example13 = Seq(Seq(addc1, addc2, addc3))
  lazy val foldingSet_example13_v2 = Seq(Seq(addc2, addc3, addc1))
}


object chap4 {
  def fig4_3 = {
    val Seq(r1, r2, r3, r4) = (0 until 4).map(i => VirtualNode[SInt](s"const$i"))
    val cg = ConstraintGraph[SInt]()
    cg.addConstraint(r1 - r2 <= 0)
    cg.addConstraint(r3 - r1 <= 5)
    cg.addConstraint(r4 - r1 <= 4)
    cg.addConstraint(r4 - r3 <= -1)
    cg.addConstraint(r3 - r2 <= 2)
    cg
  }
}

object MIMO {

  //  def fft4 = {
  //
  //    val butterfly = (dataIns: Seq[ComplexNumber], _: GlobalCount) => {
  //      val add = dataIns(0) + dataIns(1)
  //      val sub = dataIns(0) - dataIns(1)
  //      Seq(add, sub)
  //    }
  //
  //    val dfg = DFGGraph[ComplexNumber]("fft4")
  //
  //    import Operators._
  //
  //    val butterflyHardware = DSPHardware(impl = butterfly, inDegree = 2, outWidths = Seq(-1 bits, -1 bits))
  //    val alphabet = Seq("a", "b", "c", "d", "e")
  //    val butterflies = Seq.tabulate(2, 2)((i, j) => butterflyHardware.asDSPNode(s"butterfly_${alphabet(i)}${j}", 1 cycles, 1 ns)).flatten
  //    val Seq(b0, b1, c0, c1) = butterflies
  //    butterflies.foreach(dfg.addVertex)
  //
  //    Seq(b0, b1).foreach(butterfly => Seq(0, 1).foreach(dfg.setInput(butterfly, _)))
  //
  //    // new MIMO API
  //    dfg.addEdge(b0(0), c0(0), 0)
  //    dfg.addEdge(b0(1), c1(0), 0)
  //    dfg.addEdge(b1(0), c0(1), 0)
  //    dfg.addEdge(b1(1), c1(1), 0)
  //
  //    Seq(c0, c1).foreach(butterfly => Seq(0, 1).foreach((outOrder: Int) => dfg.setOutput(butterfly, outOrder = outOrder)))
  //
  //    dfg
  //  }

}