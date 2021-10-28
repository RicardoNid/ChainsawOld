package Chainsaw.Communication.channelCoding

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import org.slf4j.{Logger, LoggerFactory}
import spinal.core._
import xilinx.VivadoReport

class EncodersTest extends AnyFlatSpec {

  val logger: Logger = LoggerFactory.getLogger("testing encoders")

  def verifyConvEncoder(name: String, dut: ConvConfig => Component with DSPTestable[Vec[Bits], Vec[Bits]], convConfig: ConvConfig, testCase: Seq[BigInt]): Unit = {
    import convConfig._
    val trellisM = Refs.poly2trellisM(convConfig.ms.map(_ + 1), convConfig.codeGens)
    val golden: Array[Int] = Refs.convenc(testCase.map(_.toInt).toArray, trellisM)
    val testCases = testCase.grouped(n).toSeq
    doFlowPeekPokeTest(dut(convConfig), name, testCases, golden)
  }

  def synthConvEncoder(dut: ConvConfig => Component, convConfig: ConvConfig): VivadoReport = VivadoSynth(dut(convConfig))

  val testCases: Seq[BigInt] = (0 until 100).map(_ => DSPRand.nextBigInt(1))

  val conv802_11: ConvConfig = ConvConfig(Array(171, 133), radix = 8)
  val convMatlab: ConvConfig = ConvConfig(Array(Array(23, 35, 0), Array(0, 5, 13)), radix = 8)

  val basicEncoder: ConvConfig => ConvEncoder = ConvEncoder
  val DFGEncoder: ConvConfig => ConvEncoderDFG = ConvEncoderDFG

  "Convolutional encoder" should "work on SIMO mode(802.11)" in verifyConvEncoder("SIMO_convenc_example", basicEncoder, conv802_11, testCases)
  it should "work on MIMO mode(matlab example)" in verifyConvEncoder("MIMO_convenc_example", basicEncoder, convMatlab, testCases)
  it should "work on SIMO mode(802.11) while using DFG" in verifyConvEncoder("SIMO_convencDFG_example", DFGEncoder, conv802_11, testCases)
  it should "work on MIMO mode(matlab example) while using DFG" in verifyConvEncoder("MIMO_convencDFG_example", DFGEncoder, convMatlab, testCases)

  it should "be implemented efficiently" in {
    val impls = Seq(basicEncoder, DFGEncoder)
    val configs = Seq(conv802_11, convMatlab)
    val reports = Seq.tabulate(2,2)((i,j) => synthConvEncoder(impls(i), configs(j))).flatten
    println(reports.mkString("\n"))
  }

}
