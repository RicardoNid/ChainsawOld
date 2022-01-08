package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

class TxTest extends AnyFlatSpec {

  val testSize = 1
  val bits: Seq[BigInt] = loadFTN1d[Double]("txRaw").map(_.toInt).grouped(128).toSeq.map(bit128 => BigInt(bit128.mkString(""), 2))
  val coded: Seq[BigInt] = loadFTN1d[Double]("txCoded").map(_.toInt).grouped(256).toSeq.map(bit256 => BigInt(bit256.mkString(""), 2))
  val interleaved: Seq[BigInt] = loadFTN1d[Double]("txInterleaved").map(_.toInt).grouped(256).toSeq.map(bit256 => BigInt(bit256.mkString(""), 2))
  val mapped: Seq[Seq[BComplex]] = loadFTN1d[MComplex]("txMapped").map(_.toBComplex).grouped(256).toSeq.map(_.toSeq)
  val modulated: Seq[Seq[Double]] = loadFTN1d[Double]("txModulated").map(_ * 512.0).grouped(128).toSeq.map(_.toSeq)
  val scaled: Seq[Seq[BigInt]] = loadFTN1d[Double]("txScaled").map(_.toInt).map(BigInt(_)).grouped(128).toSeq.map(_.toSeq)

  logger.info(s"max & min: ${modulated.flatten.max}, ${modulated.flatten.min}")

  "Tx" should "work correctly on convenc and interleave" in {
    doFlowPeekPokeTest(
      name = "testTx0", dut = Tx0(channelInfo),
      testCases = Seq.fill(testSize)(bits).flatten,
      golden = Seq.fill(testSize)(interleaved).flatten,
      testMetric = TestMetric.SAME
    )
  }

  "Tx" should "work correctly on qammod" in {
    doFlowPeekPokeTest(
      name = "testTx1", dut = Tx1(channelInfo),
      testCases = Seq.fill(testSize)(bits).flatten,
      golden = Seq.fill(testSize)(mapped).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-3
    )
  }

  "Tx" should "work correctly on ifft" in {
    doFlowPeekPokeTest(
      name = "testTx", dut = Tx2(channelInfo),
      testCases = Seq.fill(testSize)(bits).flatten,
      golden = Seq.fill(testSize)(modulated).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 2E0
    )
  }

  "Tx" should "work correctly as a Whole" in {
    doFlowPeekPokeTest(
      name = "testTx", dut = TxWhole(channelInfo),
      testCases = Seq.fill(testSize)(bits).flatten,
      golden = Seq.fill(testSize)(scaled).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1
//      testMetric = TestMetric.SAME
    )
  }

  it should "synth" in VivadoSynthForTiming(TxWhole(channelInfo), "Tx")

  it should "synth for all components" in {
    import channelInfo._
    VivadoSynthForTiming(Convenc128FTN(), "convencTx")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool())), "interleaveTx")
    VivadoSynthForTiming(comm.qam.AdaptiveQammod(bitAlloc, powAlloc, symbolType), "qammodTx")
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyHSIFFT(512, 128, ifftType, symbolType, Seq(4, 4, 4, 4, 2), ifftShifts), "ifftTx")
  }
}
