package Chainsaw.comm.qam

import Chainsaw.FTN.loadFTN1d
import Chainsaw._
import Chainsaw.dspTest._
import breeze.linalg._
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.sqrt

class QamdemodWithAllocTest extends AnyFlatSpec {

  "adaptiveQamdemod" should "work" in {

    val symbolType = FTN.symbolComplexType

    val params = FTN.FtnParams(3, 226, true)
    val bitAlloc = params.channelInfo.bitAlloc
    val powAlloc = params.channelInfo.powAlloc

    val rxModulated = loadFTN1d[Double]("rxModulated").map(_ * 512.0)
    val rxModulateGolden: Seq[Seq[Double]] = {
      val (preamble, data) = rxModulated.splitAt(1024)
      preamble.grouped(512).map(_.toSeq).toSeq ++ data.grouped(450).map(_.toSeq.padTo(512, 0.0)).toSeq
    }

    val testCases: Seq[Seq[BComplex]] = rxModulateGolden.map(_.map(BComplex(_, 0.0)))
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
      "testAdaptiveDeQAM", QamdemodWithAlloc(bitAlloc, powAlloc, symbolType),
      testCases, goldens,
      testMetric = TestMetric.SAME)
  }
}
