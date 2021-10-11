package Chainsaw.DFG

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.JavaConversions._

class DFGGraphTest extends AnyFlatSpec {

  import Operators._

  "dfg" should "be implemented correctly" in {
    val dfg = DFGGraph[SInt]

    val pts = (0 until 4).map(i => sintKeep.asDSPNode(s"pt$i", 1 cycles, 1 ns))
    val Seq(pt0, pt1, pt2, pt3) = pts
    dfg.addPath(pt0 >> 1 >> pt1 >> 1 >> pt2 >> 1 >> pt3)
    dfg.setInput(pt0)
    dfg.setOutput(pt3)

    println(dfg.vertexSet().mkString(" "))
    println(dfg.edgeSet().mkString(" "))
  }

  // TODO : add these tests
  it should "work on mixed-type operators" in {
    val dfg = DFGGraph[SInt]
  }

  it should "work on unknown-width operators in forwarding DFG" in {

  }

  it should "work on partially-known-width operators in forwarding DFG" in {

  }

  val testCases = (0 until 10).map(_ => DSPRand.nextInt(4))
  //  val testCases = (0 until 20).map(_ => 1)

  def testDFG(dfg: DFGGraph[SInt], factor: Int) = {

    SimConfig.withWave.compile(new Component {
      val dataIn = in SInt (10 bits)
      val dataOut = out SInt (10 bits)
      val validIn = in Bool()
      val validOut = out Bool()
      dataOut := dfg.impl(Seq(dataIn)).head
      validOut := Delay(validIn, dfg.latency * factor, init = False)

    }).doSim { dut =>
      import dut.{clockDomain, dataIn}
      dataIn #= 0
      dut.validIn #= false
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling(10 * factor - 1)

      testCases.foreach { testCase =>
        dataIn #= testCase
        dut.validIn #= true
        clockDomain.waitSampling()
        dataIn #= 0
        dut.validIn #= false
        clockDomain.waitSampling(factor - 1)
      }

      clockDomain.waitSampling(20 * factor)
    }
  }

  // fig 6.3
  it should "fold correctly" in {
    val dfg = chap6.fig6_3
    val foldingSet = chap6.foldingSets
    val deviceGens = chap6.deviceGens

    val algo = new Folding[SInt](dfg, foldingSet, deviceGens)
    assert(new Folding[SInt](algo.retimed, foldingSet, deviceGens).isFeasible)
    val foldedDFG = algo.folded
    testDFG(dfg, 1)
    testDFG(foldedDFG, 4)
    println(algo.folded)
    DFGTestUtil.verifyFunction(dfg, foldedDFG, SInt(10 bits), -4, 0)
  }

  it should "fold correctly on simple graph" in {
    val dfg = simpleFolding.dfg
    val foldingSet = simpleFolding.foldingSets
    val deviceGens = simpleFolding.deviceGens

    val algo = new Folding[SInt](dfg, foldingSet, deviceGens)
    println(algo.folded)

    testDFG(dfg, 1)
    testDFG(algo.folded, 2)
  }

  "constraint graph" should "work on fig4.3" in {
    val cg = chap4.fig4_3
    assert(cg.getSolution.zip(Seq(0, 0, 0, -1)).forall { case (d, i) => d == i })
  }

  "critical-path-related algos" should "work on fig2.2" in { // TODO: add a test for graph with MIMO devices
    val dfg = chap2.fig2_2
    val algo = new CriticalPathAlgo(dfg)
    assert(algo.delaysCount == 4)
    assert(algo.criticalPathLength == 5.0)
    assert(algo.iterationBound == 2.0)
  }

  "unfolding algo" should "work on fig5.2" in {
    val dfg = chap5.fig5_2
    val algo = new Unfolding(dfg, 10)
    printlnGreen(new CriticalPathAlgo(dfg).iterationBound)
    val unfoldedDFG = algo.unfolded
    printlnGreen(new CriticalPathAlgo(unfoldedDFG).iterationBound)

    println(unfoldedDFG)
    DFGTestUtil.verifyFunction(dfg, unfoldedDFG, SInt(10 bits), 10, 0)
  }

  it should "work on fig5.10" in {
    val dfg = chap5.fig5_10
    val algo = new Unfolding(dfg, 3)
    val unfoldedDFG = algo.unfolded

    println(unfoldedDFG)
    DFGTestUtil.verifyFunction(dfg, unfoldedDFG, SInt(10 bits), 3, 0)
  }
}
