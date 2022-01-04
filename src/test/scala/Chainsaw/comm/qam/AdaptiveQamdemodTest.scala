package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.dspTest._
import breeze.linalg._
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.sqrt

class AdaptiveQamdemodTest extends AnyFlatSpec {

  "adaptiveQamdemod" should "work" in {

    val symbolType = FTN.unitComplexType
    val bitAlloc = FTN.channelInfo.bitAlloc
    val powAlloc = FTN.channelInfo.powAlloc

    val testCases: Seq[Seq[BComplex]] = FTN.rxEqualizedGolden
    val goldens = testCases
      .map(vec => vec.zip(bitAlloc.zip(powAlloc))
        .map { case (complex, (bit, pow)) =>
          if (bit == 0) ""
          else algos.Qam.qamdemod(new DenseVector(Array(complex / BComplex(sqrt(pow), 0.0))), 1 << bit)
            .toArray.head
            .toBinaryString.padToLeft(bit, '0')
        }.mkString(""))
      .map(BigInt(_, 2))


    doFlowPeekPokeTest(
      "testAdaptiveDeQAM", AdaptiveQamdemod(bitAlloc, powAlloc, symbolType),
      testCases, goldens,
      testMetric = TestMetric.SAME)
  }
}
