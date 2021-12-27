package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.dspTest._
import breeze.linalg._
import org.scalatest.flatspec.AnyFlatSpec

import scala.math.sqrt

class AdaptiveQamdemodTest extends AnyFlatSpec {

  "adaptiveQamdemod" should "work" in {
    val testSize = 10

    val symbolType = FTN.unitComplexType
    val bitAlloc = FTN.channelInfo.bitAlloc
    val powAlloc = FTN.channelInfo.powAlloc

    val data = (0 until testSize).map(_ => bitAlloc.map(bit => ChainsawRand.nextInt(1 << bit)))

    val testCases = data.map(d => d.zip(bitAlloc.zip(powAlloc)).map { case (bit, (size, pow)) =>
      algos.Qam.qammod(DenseVector(bit), 1 << size)(0) * BComplex(sqrt(pow), 0.0)
    }).map(_.toSeq)

    val goldens = data.map(d =>
      BigInt(d.zip(bitAlloc).map { case (bit, size) =>
        bit.toBinaryString.padToLeft(size, '0')
      }.mkString(""), 2))

    doFlowPeekPokeTest(
      "testAdaptiveDeQAM", AdaptiveQamdemod(bitAlloc, powAlloc, symbolType),
      testCases, goldens,
      testMetric = TestMetric.SAME)
  }
}
