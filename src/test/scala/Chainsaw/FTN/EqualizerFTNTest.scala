package Chainsaw.FTN

import Chainsaw.FTN.FreqEqualizerAlgo.loadData
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class EqualizerFTNTest extends AnyFlatSpec {

  behavior of "EqualizerFTNTest"

  it should "work" in {
    val testCase = rxMapped.grouped(256).toSeq.map(_.toSeq)
    val testCases = Seq.fill(3)(testCase).flatten
    val golden = rxEqualized.grouped(256).toSeq.map(_.toSeq)
    val goldens = Seq.fill(3)(golden).flatten

    doFlowPeekPokeTest(
      dut = EqualizerFTN(preambleSymbols), name = "testEqualizer",
      testCases = testCases, golden = goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }
}
