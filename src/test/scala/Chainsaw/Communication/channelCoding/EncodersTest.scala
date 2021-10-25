package Chainsaw.Communication.channelCoding

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import org.slf4j.{Logger, LoggerFactory}
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class EncodersTest extends AnyFlatSpec {

  val logger: Logger = LoggerFactory.getLogger("testing encoders")

  def verifyConvEncoder(convConfig: ConvConfig, testCase: Seq[BigInt], name: String): Unit = {
    import convConfig._
    val trellisM = Refs.poly2trellisM(convConfig.ms.map(_ + 1), convConfig.codeGens)
    val golden: Array[Int] = Refs.convenc(testCase.map(_.toInt).toArray, trellisM)
    val testCases = testCase.grouped(n).toSeq
    doFlowPeekPokeTest(ConvEncoder(convConfig), name, testCases, golden)
  }

  def verifyConvEncoderDFG(convConfig: ConvConfig, testCase: Seq[BigInt], name: String): Unit = {
    import convConfig._
    val trellisM = Refs.poly2trellisM(convConfig.ms.map(_ + 1), convConfig.codeGens)
    val golden: Array[Int] = Refs.convenc(testCase.map(_.toInt).toArray, trellisM)
    val testCases = testCase.grouped(n).toSeq
    doFlowPeekPokeTest(ConvEncoderDFG(convConfig), name, testCases, golden)
  }

  val testCases: Seq[BigInt] = (0 until 100).map(_ => DSPRand.nextBigInt(1))

  val conv802_11: ConvConfig = ConvConfig(Array(171, 133), radix = 8)
  "Convolutional encoder" should "work on SIMO mode(802.11)" in verifyConvEncoder(conv802_11, testCases, "matlab_SIMO_convenc_example")
  "Convolutional encoder" should "work on MIMO mode(matlab example)" in verifyConvEncoder(ConvConfig(Array(Array(23, 35, 0), Array(0, 5, 13)), radix = 8), testCases, "matlab_SIMO_convenc_example")
  "Convolutional encoder" should "work on SIMO mode(802.11) while using DFG" in verifyConvEncoderDFG(conv802_11, testCases, "matlab_SIMO_convencDFG_example")

}