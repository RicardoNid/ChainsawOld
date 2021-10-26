package Chainsaw.DFG

import Chainsaw.DFG.DFGTestUtil.{verifyFolding, verifyUnfolding}
import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

/** Regression test of DFG
 *
 */
class DFGGraphTest extends AnyFlatSpec {


  import Operators._

  "dfg" should "be implemented correctly" in {
    val dfg = DFGGraph[SInt]("simpleGraph")

    val pts = (0 until 4).map(i => sintKeep.asDSPNode(s"pt$i", 1 cycles, 1 ns))
    val Seq(pt0, pt1, pt2, pt3) = pts
    dfg.addPath(pt0 >> 1 >> pt1 >> 1 >> pt2 >> 1 >> pt3)
    dfg.setInput(pt0)
    dfg.setOutput(pt3)

    println(dfg.vertexSeq.mkString(" "))
    println(dfg.edgeSeq.mkString(" "))
  }

  it should "work on MIMO DFG" in {
    GenRTL(new Component {
      val dataIns: Vec[ComplexNumber] = in(Vec(ComplexNumber(1, -6), 4))
      val dataOuts: Vec[ComplexNumber] = out(Vec(ComplexNumber(1, -6), 4))
      dataOuts := Vec(MIMO.fft4.impl(dataIns))
    })
  }

  it should "work on nested DFG" in {
    println(new DFGImpl(implementingDFGs.nestedDFG).implAsComponent())
  }


  "the folding algorithm" should "fold correctly on chap6 fig6_3" in verifyFolding(chap6.fig6_3, chap6.foldingSet, "chap6_fig6_3")
  it should "fold correctly on simple graph" in verifyFolding(simpleFolding.dfg, simpleFolding.foldingSet)
  it should "fold correctly on paper1992 fig6_a(example3)" in verifyFolding(paper1992OnFolding.fig6_a, paper1992OnFolding.foldingSet_example3)
  it should "fold correctly on paper1992 fig7_a(example4)" in verifyFolding(paper1992OnFolding.fig6_a, paper1992OnFolding.foldingSet_example4)


  it should "fold correctly on paper1992 fig8_a(example6)" in {
    val dfg = paper1992OnFolding.fig8_a
    val foldingSet = paper1992OnFolding.foldingSet8_a_example6
    verifyFolding(dfg, foldingSet)
  }

  it should "fold correctly on paper1992 fig9_a" in {
    val dfg = paper1992OnFolding.fig9_a
    val foldingSet = paper1992OnFolding.foldingSet9_a_example7
    verifyFolding(dfg, foldingSet, "paper1992_fig9_a")
  }

  it should "fold correctly on paper1992 fig10_a(example8)" in {
    val dfg = paper1992OnFolding.fig10_a
    val foldingSet = paper1992OnFolding.foldingSet10_a_example8and10
    verifyFolding(dfg, foldingSet, "paper1992_fig10_a")
  }

  it should "retimed correctly on paper1992 fig10_c(example10)" in {
    val dfg = paper1992OnFolding.fig10_c
    val foldingSet = paper1992OnFolding.foldingSet10_a_example8and10
    verifyFolding(dfg, foldingSet, "paper1992_fig10_c")
  }

  it should "fold correctly on paper1992 fig12_a(example11)" in {
    val dfg = paper1992OnFolding.fig12_a
    val foldingSet = paper1992OnFolding.foldingSet_example11
    verifyFolding(dfg, foldingSet, name = "paper1992_fig12_a")
  }

  it should "fold correctly on paper1992 fig13_d" in {
    val dfg = paper1992OnFolding.fig13_a
    val foldingSet = paper1992OnFolding.foldingSet13_a_example12
    verifyFolding(dfg, foldingSet)
  }

  it should "fold correctly on paper1992 fig14_a" in {
    val dfg = paper1992OnFolding.fig14_a
    val foldingSet = paper1992OnFolding.foldingSet_example13
    verifyFolding(dfg, foldingSet, "paper1992_fig14_a")
  }

  it should "fold correctly on paper1992 fig15_a" in {
    val dfg = paper1992OnFolding.fig14_a
    val foldingSet = paper1992OnFolding.foldingSet14_a_example13_v2
    verifyFolding(dfg, foldingSet, name = "paper1992_fig15_a")
  }

  "constraint graph" should "work on fig4.3" in {
    val cg = chap4.fig4_3
    // (0,0,0,-1) => (1,1,1,0)
    assert(cg.getSolution.values.zip(Seq(1, 1, 1, 0)).forall { case (d, i) => d == i })
  }

  // FIXME: while using JVM 8(thus, JGraphT 1.4.0), this is not available
  //  "critical-path-related algos" should "work on fig2.2" in { // TODO: add a test for graph with MIMO devices
  //    val dfg = chap2.fig2_2
  //    val algo = new CriticalPathAlgo(dfg)
  //    assert(algo.delaysCount == 4)
  //    assert(algo.criticalPathLength == 5.0)
  //    assert(algo.iterationBound == 2.0)
  //  }

  "the unfolding algorithm" should "work on fig5.2" in verifyUnfolding(chap5.fig5_2, 2)
  //  it should "work on fig5.2 with innder delay" in DFGTestUtil.verifyUnfolding(chap5.fig5_2_inner_delay, 2)
  it should "work on fig5.10" in verifyUnfolding(chap5.fig5_10, 3, "fig5.10")
  it should "work on fig5.12" in verifyUnfolding(chap5.fig5_12, 2, "fig5.12")

}
