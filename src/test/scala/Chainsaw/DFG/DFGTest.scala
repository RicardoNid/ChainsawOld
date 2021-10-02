package Chainsaw.DFG

import org.scalatest.flatspec.AnyFlatSpec

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._

class DFGTest extends AnyFlatSpec {

  import Operators._

  "dfg" should "be implemented correctly" in {
    val dfg = DFG[SInt]

    val pts = (0 until 4).map(i => SIntPT.asDSPNode(s"pt$i", 1 cycles, 1 ns))
    val Seq(pt0, pt1, pt2, pt3) = pts
    dfg.addPath(pt0 >> 1 >> pt1 >> 1 >> pt2 >> 1 >> pt3)
    dfg.setInput(pt0)
    dfg.setOutput(pt3)

    println(dfg.vertexSet().mkString(" "))
    println(dfg.edgeSet().mkString(" "))

    //    GenRTL(new Component {
    //      val dataIn = in SInt (4 bits)
    //      val dataOut = out SInt (4 bits)
    //
    //      dataOut := dfg.impl(Seq(dataIn)).head
    //    })
  }

  val testCases = (0 until 10).map(_ => DSPRand.nextInt(4))
  //  val testCases = (0 until 20).map(_ => 1)

  def testDFG(dfg: DFG[SInt], factor: Int) = {

    SimConfig.withWave.compile(new Component {
      val dataIn = in SInt (10 bits)
      val dataOut = out SInt (10 bits)
      dataOut := dfg.impl(Seq(dataIn)).head

    }).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      dataIn #= 0
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling(10 * factor - 1)

      printlnBlue(s"factor = $factor")
      testCases.foreach { testCase =>
        dataIn #= testCase
        clockDomain.waitSampling()
        dataIn #= 0
        printlnBlue(s"wait ${factor - 1}")
        clockDomain.waitSampling(factor - 1)
      }

      clockDomain.waitSampling(20 * factor)
    }
  }

  // fig 6.3
  it should "fold correctly" in {
    val dfg = chap6.dfg6_3
    val foldingSet = chap6.foldingSets
    val deviceGens = chap6.deviceGens

    val algo = new Folding[SInt](dfg, foldingSet, deviceGens)
    assert(new Folding[SInt](algo.retimed, foldingSet, deviceGens).isFeasible)
    testDFG(dfg, 1)
    testDFG(algo.folded, 4)
    println(algo.folded)
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
    assert(cg.getSolution.zip(Seq(0,0,0,-1)).forall{ case (d, i) => d == i})
  }
}
