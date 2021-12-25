package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec

class TxFrontTest extends AnyFlatSpec {

  def loadFTN[T](name: String) = eng.load[T](s"~/FTN326/$name.mat", name)

  val bitAlloc = loadFTN[Array[Double]]("bitAlloc").map(_.toInt)
  logger.info(s"bitAlloc: ${bitAlloc.mkString(" ")}")
  val powAlloc = loadFTN[Array[Double]]("powAlloc").map(pow => pow * pow)
  logger.info(s"powAlloc: ${powAlloc.mkString(" ")}")
  val qamPositions = loadFTN[Array[Double]]("QAMPositions").map(_.toInt)
  logger.info(s"qam position: ${qamPositions.mkString(" ")}")
  val qamRemapPositions = loadFTN[Array[Double]]("QAMRemapPositions").map(_.toInt)
  logger.info(s"qamRemap position: ${qamRemapPositions.mkString(" ")}")
  val bits = eng.load[Array[Array[Double]]]("~/FTN326/bitsAllFrame.mat", "bitsAllFrame")
    .transpose.flatten.map(_.toInt)
  val bitMask = loadFTN[Array[Double]]("bitMask").map(_.toInt)
  logger.info(s"bitMask: ${bitMask.mkString(" ")}")

  val coded = eng.load[Array[Array[Double]]]("~/FTN326/codedBitsAllFrame.mat", "codedBitsAllFrame")
    .transpose.flatten.map(_.toInt)

  val interleaved = eng.load[Array[Array[Double]]]("~/FTN326/interleavedBitsAllFrame.mat", "interleavedBitsAllFrame")
    .transpose.flatten.map(_.toInt)

  val ret = eng.load[Array[Array[MComplex]]]("~/FTN326/mappedSymbolsAllFrame.mat", "mappedSymbolsAllFrame")
    .transpose.flatten.map(_.toBComplex)

  def printHex(string: String) = BigInt(string,2).toString(16)

  printlnGreen(printHex(bits.slice(128, 256).mkString("")))
  printlnGreen("coded")
  printlnGreen(printHex(coded.slice(256, 512).mkString("")))
  printlnGreen(printHex(interleaved.slice(256, 512).mkString("")))

  val testCases: Seq[BigInt] = bits.grouped(128).toSeq.map(bit128 => BigInt(bit128.mkString(""), 2))
  val goldens: Seq[Seq[BComplex]] = ret.toSeq.grouped(64).toSeq

  doFlowPeekPokeTest(
    name = "testTx", dut = TxFront(bitAlloc, powAlloc, bitMask, qamPositions, qamRemapPositions),
    testCases = testCases,
    golden = goldens,
    initLength = 0,
    testMetric = TestMetric.APPROXIMATE,
    epsilon = 1E-2
  )
}
