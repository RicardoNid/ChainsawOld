package Chainsaw.FTN

import Chainsaw.FTN.FreqEqualizerAlgo.loadData
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class EqualizerFTNTest extends AnyFlatSpec {

  behavior of "EqualizerFTNTest"

  val (preamble, data, goldenSymbols, goldenFactors, goldenResults) = loadData()

  it should "work" in {
    val testCase = (preamble ++ data).map(_.toArray.toSeq)
    val testCases = Seq.fill(3)(testCase).flatten
    val golden = equalized.grouped(256).toSeq.map(_.toSeq)
    val goldens = Seq.fill(3)(golden).flatten

    doFlowPeekPokeTest(
      dut = EqualizerFTN(preambleSymbols), name = "testEqualizer",
      testCases = testCases, golden = goldens,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1
    )
  }
}
