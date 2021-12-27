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
  val modulated = loadFTN2d[Double]("recvModulatedSymbolsAllFrame").map(_ * 512.0)
  val deModulated = loadFTN2d[MComplex]("deModulatedSymbolsAllFrame").map(_.toBComplex)
  val equalized = loadFTN2d[MComplex]("equalizedSymbolsAllFrame").map(_.toBComplex)
  val deMapped = loadFTN2d[Double]("deMappedBitsAllFrame").map(_.toInt)
  val deInterleaved = loadFTN2d[Double]("deInterleavedBitsAllFrame").map(_.toInt)
  val deCoded = loadFTN2d[Double]("debitsAllFrame").map(_.toInt)

  // transform data to testCases
  val testCases: Seq[Seq[Double]] = modulated.grouped(450).map(_.toSeq.padTo(512, 0.0)).toSeq
  logger.info(s"number of testCases: ${testCases.length}")
  val deModulatedGolden: Seq[Seq[BComplex]] = deModulated.grouped(256).map(_.toSeq).toSeq
  val equalizedGolden: Seq[Seq[BComplex]] = equalized.grouped(256).map(_.toSeq).toSeq
  val deMappedGolden: Seq[BigInt] = deMapped.grouped(1024).toSeq.map(bit1024 => BigInt(bit1024.mkString(""), 2))
  val deInterleavedGolden: Seq[BigInt] = deMapped.grouped(1024).toSeq.map(bit1024 => BigInt(bit1024.mkString(""), 2))

  println(equalized(0))
  println(deModulated(0))
  logger.info(s"data generation completed")

  // simulations
  "Rx" should "work correctly on fft" in {
    doFlowPeekPokeTest(
      dut = Rx0(channelInfo), name = "testRx",
      testCases = testCases, golden = deModulatedGolden,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE, epsilon = 5E-2
    )
  }

  "Rx" should "work correctly on qamdemod" in {
    doFlowPeekPokeTest(
      dut = Rx1(channelInfo), name = "testRx",
      testCases = testCases, golden = deMappedGolden,
      initLength = 0,
      testMetric = TestMetric.SAME
    )
  }

  it should "work correctly on interleave" in {
    doFlowPeekPokeTest(
      dut = Rx1(channelInfo), name = "testRx",
      testCases = testCases, golden = deMappedGolden,
      initLength = 0,
      testMetric = TestMetric.SAME
    )
  }

  "Rx" should "load data" in {

  }
}
