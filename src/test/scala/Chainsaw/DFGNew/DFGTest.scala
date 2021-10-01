package Chainsaw.DFGNew

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

  // fig 6.3
  it should "fold correctly" in {
    val dfg = fig6_3.dfgBeforeRetiming
    val foldingSet = fig6_3.foldingSets
    val deviceGens = fig6_3.deviceGens

    val algo = new Folding[SInt](dfg, foldingSet, deviceGens)

    printlnGreen("retiming solutions")
    println(algo.solveRetiming().mkString(" "))
    val retimedDfg = algo.retimed

    printlnGreen("fig 6.3 before retiming")
    println(dfg)
    println(s"${dfg.delayAmount} delays in total")
    printlnGreen("fig 6.3(retimed for folding)")
    println(retimedDfg)
    println(s"${retimedDfg.delayAmount} delays in total")
    assert(new Folding[SInt](retimedDfg, foldingSet, deviceGens).isFeasible)

    val foldedDFG = algo.folded
    printlnGreen("folded")
    println(foldedDFG)


    val testCases = (0 until 20).map(_ => DSPRand.nextInt(8))

    def testDFG(dfg:DFG[SInt], factor:Int) = {

      SimConfig.withWave.compile(new Component {
        val dataIn = in SInt (4 bits)
        val dataOut = out SInt (4 bits)
        val counter = CounterFreeRun(dfg.globalLcm)
        dataOut := dfg.impl(Seq(dataIn), counter.value).head

      }).doSim { dut =>
        import dut.{clockDomain, dataIn, dataOut}
        dataIn #= 0
        clockDomain.forkStimulus(2)
        clockDomain.waitSampling(10)

        testCases.foreach { testCase =>
          dataIn #= testCase
          clockDomain.waitSampling(factor)
        }
      }
    }

    testDFG(dfg, 1)
    testDFG(foldedDFG, 4)

    //        val golden = fig6_3.dfg
    //        printlnGreen("golden")
    //        println(golden)
    //        println(s"${golden.delayAmount} delays in total")

  }
}
