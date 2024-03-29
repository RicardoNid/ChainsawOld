package Chainsaw.DFG

import Chainsaw.DFG.DFGTestUtil.{verifyFolding, verifyUnfolding}
import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

/** Regression test of DFG
  */
class DFGGraphTest extends AnyFlatSpec {

  import Operators._

  globalImplPolicy = testImplPolicy

  val testHardType: HardType[SInt] = HardType(SInt(10 bits))

  lazy val textBookDUTs: Seq[DFGGraph[SInt]] =
    Seq(chap2.fig2_2, chap5.fig5_2, chap5.fig5_2_inner_delay, chap5.fig5_10, chap5.fig5_12, chap6.fig6_3, chap6.fig6_5)

  lazy val paper1992OnFoldingDUTs: Seq[DFGGraph[SInt]] = Seq(
    paper1992OnFolding.fig6_a,
    paper1992OnFolding.fig8_a,
    paper1992OnFolding.fig9_a,
    paper1992OnFolding.fig10_a,
    paper1992OnFolding.fig10_c,
    paper1992OnFolding.fig12_a,
    paper1992OnFolding.fig13_a,
    paper1992OnFolding.fig14_a
  )

  it should "work on nested DFG" in {
    val nested = implementingDFGs.nestedDFG
    genDFG(nested, Seq.fill(nested.inputNodes.size)(10 bits))
  }

  "dfg" should "be implemented correctly" in {
    val dfg = DFGGraph[SInt]("simpleGraph")

    val pts                     = (0 until 4).map(i => BinaryHardware(sintMult, 10 bits, 0 cycles, 1 ns).asDeviceNode(s"pt$i"))
    val ptsCNodes               = (0 until 4).map(i => ConstantNode[SInt, Int](s"ptscnode_${i + 1}", 1, 10 bits))
    val Seq(pt0, pt1, pt2, pt3) = pts
    dfg.addPath(pt0 >> 1 >> pt1 >> 1 >> pt2 >> 1 >> pt3)
    ptsCNodes.zip(pts).foreach { case (pcnode, pnode) => dfg.addPath(pcnode >> 0 >> pnode) }
    dfg.setInput(pt0)
    dfg.setOutput(pt3)

    println(dfg.vertexSeq.mkString(" "))
    println(dfg.edgeSeq.mkString(" "))
  }

  //  it should "work on MIMO DFG" in {
  //    GenRTL(new Component {
  //      val dataIns: Vec[ComplexNumber] = in(Vec(ComplexNumber(1, -6), 4))
  //      val dataOuts: Vec[ComplexNumber] = out(Vec(ComplexNumber(1, -6), 4))
  //      dataOuts := Vec(MIMO.fft4.impl(dataIns))
  //    })
  //  }

  "the folding algorithm" should "fold correctly on chap6 fig6_4" in verifyFolding(chap6.fig6_3, chap6.foldingSet, testHardType, "chap6_fig6_3")
  "the folding algorithm" should "also fold correctly on chap6 fig6_5" in verifyFolding(chap6.fig6_5, chap6.foldingSet, testHardType, "chap6_fig6_5")
  it should "fold correctly on simple graph" in verifyFolding(simpleFolding.dfg, simpleFolding.foldingSet, testHardType)
  it should "fold correctly on paper1992 fig6_b(example3)" in verifyFolding(paper1992OnFolding.fig6_a, paper1992OnFolding.foldingSet_example3, testHardType)
  it should "fold correctly on paper1992 fig7_a(example4)" in verifyFolding(paper1992OnFolding.fig6_a, paper1992OnFolding.foldingSet_example4, testHardType)

  it should "fold correctly on paper1992 fig8_b(example6)" in {
    val dfg        = paper1992OnFolding.fig8_a
    val foldingSet = paper1992OnFolding.foldingSet_example6
    verifyFolding(dfg, foldingSet, testHardType)
  }

  it should "fold correctly on paper1992 fig9_b(example7)" in {
    val dfg        = paper1992OnFolding.fig9_a
    val foldingSet = paper1992OnFolding.foldingSet_example7
    verifyFolding(dfg, foldingSet, testHardType, "paper1992_fig9_a")
  }

  it should "fold correctly on paper1992 fig10_b(example8)" in {
    val dfg        = paper1992OnFolding.fig10_a
    val foldingSet = paper1992OnFolding.foldingSet_example8and10
    verifyFolding(dfg, foldingSet, testHardType, "paper1992_fig10_a")
  }

  it should "fold correctly on paper1992 fig10_b(example10)" in {
    val dfg        = paper1992OnFolding.fig10_c
    val foldingSet = paper1992OnFolding.foldingSet_example8and10
    verifyFolding(dfg, foldingSet, testHardType, "paper1992_fig10_c")
  }

  it should "fold correctly on paper1992 fig12_a(example11)" in {
    val dfg        = paper1992OnFolding.fig12_a
    val foldingSet = paper1992OnFolding.foldingSet_example11
    verifyFolding(dfg, foldingSet, testHardType, name = "paper1992_fig12_a")
  }

  // skip this as no I/O specified
  //  it should "fold correctly on paper1992 fig13_d(example12)" in {
  //    val dfg = paper1992OnFolding.fig13_a
  //    val foldingSet = paper1992OnFolding.foldingSet_example12
  //    verifyFolding(dfg, foldingSet, testHardType)
  //  }

  it should "fold correctly on paper1992 fig14_a(example13_v1)" in {
    val dfg        = paper1992OnFolding.fig14_a
    val foldingSet = paper1992OnFolding.foldingSet_example13
    verifyFolding(dfg, foldingSet, testHardType, "paper1992_fig14_a")
  }

  it should "fold correctly on paper1992 fig15_a(example13_v2)" in {
    val dfg        = paper1992OnFolding.fig14_a
    val foldingSet = paper1992OnFolding.foldingSet_example13_v2
    verifyFolding(dfg, foldingSet, testHardType, name = "paper1992_fig15_a")
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

  "the unfolding algorithm" should "work on fig5.2" in verifyUnfolding(chap5.fig5_2, 2, testHardType)
  //  it should "work on fig5.2 with innder delay" in DFGTestUtil.verifyUnfolding(chap5.fig5_2_inner_delay, 2)
  it should "work on fig5.10" in verifyUnfolding(chap5.fig5_10, 3, testHardType, "fig5.10")
  it should "work on fig5.12" in verifyUnfolding(chap5.fig5_12, 2, testHardType, "fig5.12")

  "the reg opt algorithms" should "work on multiple different graphs" in {

    val duts0 = textBookDUTs.filterNot(_.isMerged)
    logger.info(
      s"testing reg merge on\n${duts0.map(_.name).mkString(" ")}" +
        s"\n${duts0.map(dut => s"${dut.name}: ${dut.unmergedDelayAmount} -> ${dut.merged.unmergedDelayAmount}").mkString("\n")}"
    )
    assert(duts0.forall(_.merged.isMerged))

    val duts1 = paper1992OnFoldingDUTs.filterNot(_.isMerged)
    logger.info(
      s"testing reg merge on\n${duts1.filterNot(_.isMerged).map(_.name).mkString(" ")}" +
        s"\n${duts1.map(dut => s"${dut.name}: ${dut.unmergedDelayAmount} -> ${dut.merged.unmergedDelayAmount}").mkString("\n")}"
    )
    assert(duts1.forall(_.merged.isMerged))
  }

  "DFG MUX" should "be implemented correctly" in {
    GenRTL(
      new Component {
        val dataIn: Vec[Bits]                 = in Vec (Bits(4 bits), 3)
        val count: UInt                       = in UInt (log2Up(24) bits)
        implicit val globalCount: GlobalCount = GlobalCount(count)
        val dataOut: Bits                     = out Bits (4 bits)

        val mux: DFGMUX[Bits] = DFGMUX[Bits](
          Seq(
            Seq(Schedule(1, 4), Schedule(2, 4)),
            Seq(Schedule(0, 8), Schedule(4, 8)),
            Seq(Schedule(3, 12), Schedule(7, 12), Schedule(11, 12))
          ),
          24
        )

        dataOut := mux.impl(dataIn)
      },
      name = "complexMUX"
    )

    GenRTL(new Component {
      val dataIn: Bits                      = in Bits (4 bits)
      val count: UInt                       = in UInt (log2Up(24) bits)
      implicit val globalCount: GlobalCount = GlobalCount(count)
      val dataOut: Bits                     = out Bits (4 bits)

      val mux: DFGMUX[Bits] = DFGMUX[Bits](Seq(Seq(Schedule(1, 1))), 24)
      dataOut := mux.impl(Seq(dataIn))
    })
  }

  //  "DSPNode" should "have a correct inferable property" in {
  //    println(crypto.lattice.HuaweiKyber.ctButterflyNode.hardware.inferable)
  //    println(BinaryNode(Operators.sintMult, "mult").hardware.inferable)
  //    println(BinaryNode(sintMult, s"cmult", 10 bits, 0 cycles, 2 ns).hardware.inferable)
  //  }

}
