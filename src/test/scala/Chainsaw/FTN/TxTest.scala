package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec

class TxTest extends AnyFlatSpec {

  val bits = loadFTN2d[Double]("bitsAllFrame").map(_.toInt)
  val coded = loadFTN2d[Double]("codedBitsAllFrame").map(_.toInt)
  val interleaved = loadFTN2d[Double]("interleavedBitsAllFrame").map(_.toInt)
  val bitsReordered = loadFTN2d[Double]("bitsReordered").map(_.toInt)
  val mapped = loadFTN2d[MComplex]("mappedSymbolsAllFrame").map(_.toBComplex)
  val ret = loadFTN2d[Double]("modulatedSymbolsAllFrame").map(_ * 512.0)

  logger.info(s"max & min: ${ret.max}, ${ret.min}")

  val testCases: Seq[BigInt] = bits.grouped(128).toSeq.map(bit128 => BigInt(bit128.mkString(""), 2))
  val goldenCoded: Seq[BigInt] = coded.grouped(256).toSeq.map(bit256 => BigInt(bit256.mkString(""), 2))
  val goldenInterleaved: Seq[BigInt] = interleaved.grouped(256).toSeq.map(bit256 => BigInt(bit256.mkString(""), 2))
  val goldenReordered: Seq[BigInt] = bitsReordered.grouped(1024).toSeq.map(bit1024 => BigInt(bit1024.mkString(""), 2))
  val goldenMapped: Seq[Seq[BComplex]] = mapped.toSeq.grouped(256).toSeq
  val goldens: Seq[Seq[Double]] = ret.toSeq.grouped(128).toSeq

  "Tx" should "work" in {
    doFlowPeekPokeTest(
      name = "testTx", dut = Tx(channelInfo),
      testCases = testCases ++ testCases,
      golden = goldens ++ goldens,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE,
      epsilon = 1E-1
    )
  }

  it should "synth" in {
    VivadoSynth(Tx(channelInfo), "Tx")
    //    VivadoSynth(DSP.FFT.CooleyTukeyBackToBack(512, 128, Seq(4,4,4,2), Seq(4), true, dataType, unitType), "TxIfft")
  }
}
