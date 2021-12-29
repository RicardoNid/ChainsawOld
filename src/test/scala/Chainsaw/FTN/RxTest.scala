package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class RxTest extends AnyFlatSpec {

  // loading data

  // transform data to testCases
  println(equalized(0))
  println(deModulated(0))
  logger.info(s"data generation completed")

  // simulations
  "Rx" should "work correctly on fft" in {
    doFlowPeekPokeTest(
      dut = Rx0(channelInfo), name = "testRx",
      testCases = modulateGolden, golden = deModulatedGolden,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE, epsilon = 5E-2
    )
  }

  it should "work correctly on qamdemod" in {
    doFlowPeekPokeTest(
      dut = Rx1(channelInfo), name = "testRx",
      testCases = modulateGolden, golden = deMappedGolden,
      initLength = 0,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on interleave" in {
    doFlowPeekPokeTest(
      dut = Rx2(channelInfo), name = "testRx",
      testCases = modulateGolden, golden = deInterleavedGolden,
      initLength = 0,
      testMetric = TestMetric.SAME
    )
  }

  "Rx" should "load data" in {

  }
}
