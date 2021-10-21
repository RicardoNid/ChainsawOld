package Chainsaw.DFG

import Chainsaw.DFG.Operators._
import Chainsaw._
import spinal.core._

import scala.language.postfixOps

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

object implementingDFGs {
  def nestedDFG = {
    val butterfly = DFGGraph[SInt]
    val add0 = SIntAdder("add0", 10 bits, 1 cycles, 1 ns)
    val add1 = SIntAdder("add1", 10 bits, 1 cycles, 1 ns)
    butterfly.addVertex(add0)
    butterfly.addVertex(add1)
    val in0 = butterfly.addInput("in0")
    val in1 = butterfly.addInput("in1")
    butterfly.addEdge(in0(0), add0(0), 0)
    butterfly.addEdge(in0(0), add1(1), 0)
    butterfly.addEdge(in1(0), add0(1), 0)
    butterfly.addEdge(in1(0), add1(0), 0)
    butterfly.setOutput(add0)
    butterfly.setOutput(add1)

    println(butterfly)
    val whole = DFGGraph[SInt]
    whole.addVertex(butterfly.asNode)
    whole.addVertex(butterfly.asNode)
    whole
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

    dfg.addEdge(add, add, out2add)
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

  val foldingSet = Seq(
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

  /*  -----------------------------------fig1_example1------------------------------------*/


  /*  -----------------------------------fig6_a_example3------------------------------------*/

  val cmuls = (0 until 4).map(i => SIntCMult(s"cmult_$i", 2 * i + 1, 10 bits, 2 cycles, 1 ns))
  val Seq(cmul0, cmul1, cmul2, cmul3) = cmuls


  def fig6_a = {
    val dfg = DFGGraph[SInt]
    cmuls.foreach(dfg.addVertex(_))
    dfg.addPath(cmul0 >> 0 >> cmul1 >> 1 >> cmul2 >> 2 >> cmul3)
    dfg.setInput(cmul0)
    dfg.setOutput(cmul3)
    dfg
  }

  def foldingSet6_a_example3 = Seq(Seq(cmul0, cmul1), Seq(cmul2, cmul3))

  /*  -----------------------------------fig6_a_example4------------------------------------*/

  def foldingSet6_a_example4 = Seq(Seq(cmul0, cmul2), Seq(cmul1, cmul3))

  /*  -----------------------------------fig8_a_example6------------------------------------*/


  val cmults = (0 until 5).map(i => SIntCMult(s"cmult_$i", i + 1, 10 bits, 0 cycles, 2 ns))
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

  def foldingSet8_a_example6 = Seq(Seq(cmult0, cmult1), Seq(cmult3, cmult2), Seq(cmult4, null))
  //  def foldingSet8_a = Seq(Seq(cmult0, cmult1), Seq(cmult2, cmult3), Seq(cmult4, null))

  /*  -----------------------------------fig9_a_example7------------------------------------*/

  val fig9Nodes = (0 until 5).map(i => SIntCMulAdder(s"ma${i + 1}", i % 3 + 1, 10 bits, 0 cycles, 1 ns))
  val Seq(ma1, ma2, ma3, ma4, ma5) = fig9Nodes
  val zero = SIntConst(s"zero", 0, 10 bits)
  val constantNodes = (0 until 5).map(i => SIntConst(s"a_$i", i + 2, 10 bits))

  val sKeeps = (0 until 5).map(i => SIntCMult(s"skeep_$i", 1, 10 bits, 0 cycles, 0 ns))
  val Seq(sk0, sk1, sk2, sk3, sk4) = sKeeps

  def fig9_a: DFGGraph[SInt] = {
    val dfg = DFGGraph[SInt]()
    fig9Nodes.foreach(dfg.addVertex(_))
    dfg.addVertex(zero)
    val x_n = dfg.addInput(s"x_n")
    dfg.setOutput(ma5, 0, s"y_n")

    fig9Nodes.foreach(node => dfg.addEdge(x_n(0), node(0), 0))
    dfg.addPath(zero >> ma1 >> 1 >> ma2 >> 1 >> ma3 >> 1 >> ma4 >> 1 >> ma5)

    dfg
  }

  def foldingSet9_a_example7 = Seq(
    Seq(ma1, ma2, ma3),
    Seq(ma4, ma5, null))

  /*  -----------------------------------fig10_a_example8------------------------------------*/

  val fig10Nodes = (0 until 4).map(i => SIntCMult(s"A${i + 1}", i + 1, 10 bits, 0 cycles, 1 ns))
  val Seq(a1, a2, a3, a4) = fig10Nodes
  val foldingSet = Seq(Seq(a1, a2, a3, a4))

  def fig10 = {

    val fig10a = DFGGraph[SInt]()
    fig10Nodes.foreach(fig10a.addVertex(_))
    val x_1 = fig10a.setInput(a1, 0, s"x_1", Seq(Schedule(0, 2)))
    val x_2 = fig10a.setInput(a1, 0, s"x_2", Seq(Schedule(1, 2)))
    fig10a.addPath(a1 >> 1 >> a2 >> 1 >> a3)
    fig10a.addEdge(a1, a4, 2, Seq(Schedule(1, 3)))
    fig10a.addEdge(a3, a4, 1, Seq(Schedule(0, 3), Schedule(2, 3)))
    fig10a.setOutput(a4)

    val fig10c = fig10a.clone().asInstanceOf[DFGGraph[SInt]]
    fig10c.setEdgeWeight(fig10c.getEdge(a1, a2), 0)
    fig10c.setEdgeWeight(fig10c.getEdge(a2, a3), 0)
    fig10c.removeEdge(fig10c.getEdge(a1, a4))
    fig10c.removeEdge(fig10c.getEdge(a3, a4))
    fig10c.addEdge(a1, a4, 0, Seq(Schedule(2, 3)))
    fig10c.addEdge(a3, a4, 1, Seq(Schedule(0, 3), Schedule(1, 3)))

    (fig10a, fig10c, foldingSet)
  }

  def fig10_a = fig10._1

  def fig10_c = fig10._2

  def foldingSet10_a_example8and10 = fig10._3


  /*  -----------------------------------fig10_c_example10------------------------------------*/


  /*  -----------------------------------fig12_a_example11------------------------------------*/


  val fig12Ms = (0 until 4).map(i => SIntCMult(s"M${i + 1}", i + 1, 10 bits, 0 cycles, 1 ns))
  val fig12As = (0 until 4).map(i => SIntAdder(s"A${i + 1}", 10 bits, 0 cycles, 1 ns))
  val Seq(a_1, a_2, a_3, a_4) = fig12As
  val Seq(m1, m2, m3, m4) = fig12Ms

  def foldingSet_example11 = Seq(Seq(a_4, a_2, null, a_3, a_1), Seq(m1, m3, m2, m4, null))

  def fig12_a = {
    val dfg = DFGGraph[SInt]
    (fig12As ++ fig12Ms).foreach(dfg.addVertex(_))
    dfg.setInput(a_1)
    dfg.addPath(a_1 >> a_2)
    dfg.setOutput(a_2)
    dfg.addPath(a_1 >> 1 >> m1 >> a_3 >> a_1)
    dfg.addPath(a_1 >> 1 >> m2 >> a_4 >> a_2)
    dfg.addPath(a_1 >> 2 >> m3 >> a_3)
    dfg.addPath(a_1 >> 2 >> m4 >> a_4)
    dfg
  }



  /*  -----------------------------------fig13_a_example12------------------------------------*/

  // add two inputs and one output to fig13_a
  val cMuls = (0 until 2).map(i => SIntCMult(s"cmult_$i", i + 1, 10 bits, 2 cycles, 1 ns))
  val Seq(cMul0, cMul1) = cMuls
  val sAdds = (0 until 2).map(i => SIntAdder(s"cadder_$i", 10 bits, 1 cycles, 1 ns))
  val Seq(sAdd0, sAdd1) = sAdds

  def fig13_a = {
    val dfg = DFGGraph[SInt]
    cMuls.foreach(dfg.addVertex(_))
    sAdds.foreach(dfg.addVertex(_))
    dfg.addPath(sAdd0 >> 1 >> cMul0 >> 1 >> sAdd1 >> 1 >> cMul1 >> 2 >> sAdd0)
    dfg.setInput(sAdd0, 1, s"input_0")
    dfg.setInput(sAdd1, 1, s"input_1")
    dfg.setOutput(cMul1, 0, s"output_0")
    dfg
  }

  def foldingSet13_a_example12 = Seq(Seq(sAdd0, sAdd1), Seq(cMul0, cMul1))

  /*  -----------------------------------fig14_a_example13------------------------------------*/

  val fig14ACs = (0 until 3).map(i => SIntAdderC(s"A${i + 1}", 10 bits, 0 cycles, 1 ns))
  val Seq(ac1, ac2, ac3) = fig14ACs

  def fig14_a: DFGGraph[SInt] = {
    val dfg = DFGGraph[SInt]()
    fig14ACs.foreach(dfg.addVertex(_))
    dfg.addVertex(zero)
    // zero => adder
    fig14ACs.zipWithIndex.foreach{ case (ac, i) => dfg.addEdge(zero(0), ac(2), 0, Seq(Schedule(i, 4)))}
    // carry => next adder
    dfg.addEdge(ac1(1), ac2(2), 0, Seq(Schedule(0, 4), Schedule(2, 4), Schedule(3, 4)))
    dfg.addEdge(ac2(1), ac3(2), 0, Seq(Schedule(0, 4), Schedule(1, 4), Schedule(3, 4)))
    dfg.addEdge(ac3(1), ac1(2), 1, Seq(Schedule(1, 4), Schedule(2, 4), Schedule(3, 4)))
    // input => adder
    fig14ACs.zipWithIndex.foreach{ case (ac, i) => dfg.setInput(ac, 0, s"x_3k_$i")}
    fig14ACs.zipWithIndex.foreach{ case (ac, i) => dfg.setInput(ac, 1, s"y_3k_$i")}
    fig14ACs.zipWithIndex.foreach{ case (ac, i) => dfg.setOutput(ac, 0, s"s_3k_$i")}

    dfg
  }

  def foldingSet_example13 = Seq(Seq(ac1, ac2, ac3))
  def foldingSet14_a_example13_v2 = Seq(Seq(ac2, ac3, ac1))
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