package Chainsaw.FTN

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.reflect.ClassTag

class TxTest extends AnyFlatSpec {

  def loadFTN1d[T](name: String) = eng.load[Array[T]](s"~/FTN326/$name.mat", name)
  
  def loadFTN2d[T](name: String)(implicit tag: ClassTag[T]) =
    eng.load[Array[Array[T]]](s"~/FTN326/$name.mat", name).transpose.flatten
  
  val bitAlloc = loadFTN1d[Double]("bitAlloc").map(_.toInt)
  val bitMask = loadFTN1d[Double]("bitMask").map(_.toInt)
  val powAlloc = loadFTN1d[Double]("powAlloc").map(pow => pow * pow)
  val qamPositions = loadFTN1d[Double]("QAMPositions").map(_.toInt).map(_ - 1)
  val qamRemapPositions = loadFTN1d[Double]("QAMRemapPositions").map(_.toInt).map(_ - 1)
  
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
      name = "testTx", dut = Tx(bitAlloc, powAlloc, bitMask, qamPositions, qamRemapPositions),
      testCases = testCases ++ testCases,
      golden = goldens ++ goldens,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE,
      epsilon = 1E-2
    )
  }

  it should "synth" in {
    VivadoSynth(Tx(bitAlloc, powAlloc, bitMask, qamPositions, qamRemapPositions))
  }
}
