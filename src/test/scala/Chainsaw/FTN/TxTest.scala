package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

class TxTest extends AnyFlatSpec {

  val testSize = 1
  val bits = loadFTN1d[Double]("txRaw").map(_.toInt)
  val ret = loadFTN1d[Double]("txModulated").map(_ * 512.0)

  logger.info(s"max & min: ${ret.max}, ${ret.min}")

  val testCases: Seq[BigInt] = bits.grouped(128).toSeq.map(bit128 => BigInt(bit128.mkString(""), 2))
  val goldens: Seq[Seq[Double]] = ret.toSeq.grouped(128).toSeq

  "Tx" should "work" in {
    doFlowPeekPokeTest(
      name = "testTx", dut = Tx(channelInfo),
      testCases = Seq.fill(testSize)(testCases).flatten,
      golden = Seq.fill(testSize)(goldens).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E0
    )
  }

  it should "synth" in VivadoSynthForTiming(Tx(channelInfo), "Tx")

  it should "synth for all components" in {
    import channelInfo._
    VivadoSynthForTiming(Convenc128FTN(), "convencTx")
    VivadoSynthForTiming(DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool())), "interleaveTx")
    VivadoSynthForTiming(comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType), "qammodTx")
    cmultConfig = ComplexMultConfig(fast = true, 3, ifftType)
    VivadoSynthForTiming(DSP.FFT.CooleyTukeyHSIFFT(512, Seq(4, 4, 4), Seq(4, 2), ifftType, unitType), "ifftTx")
  }
}
