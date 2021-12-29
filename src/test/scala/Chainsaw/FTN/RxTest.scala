package Chainsaw.FTN

import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class RxTest extends AnyFlatSpec {

  val testSize = 1
  val data = (0 until testSize).flatMap(_ => modulateGolden)
  val dataWithPreamble = (0 until testSize).flatMap(_ => preambleModulatedGolden ++ modulateGolden)

  // simulations
  "Rx" should "work correctly on fft" in {
    val goldens = (0 until testSize).flatMap(_ => deModulatedGolden)
    doFlowPeekPokeTest(
      dut = Rx0(channelInfo), name = "testRxFft",
      testCases = data, golden = goldens,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE, epsilon = 5E-2
    )
  }

  it should "work correctly on equalization" in {
    val goldens = (0 until testSize).flatMap(_ => equalizedGolden)
    doFlowPeekPokeTest(
      dut = Rx1(channelInfo), name = "testRxEqual",
      testCases = dataWithPreamble, golden = goldens,
      initLength = 0,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on qamdemod" in {
    val goldens = (0 until testSize).flatMap(_ => deMappedGolden)
    doFlowPeekPokeTest(
      dut = Rx2(channelInfo), name = "testRxQam",
      testCases = dataWithPreamble, golden = goldens,
      initLength = 0,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on interleave" in {
    val goldens = (0 until testSize).flatMap(_ => deInterleavedGolden)
    doFlowPeekPokeTest(
      dut = Rx3(channelInfo), name = "testRxInter",
      testCases = dataWithPreamble, golden = goldens,
      initLength = 0,
      testMetric = TestMetric.SAME
    )
  }

  "Rx" should "load data" in {

  }
}
