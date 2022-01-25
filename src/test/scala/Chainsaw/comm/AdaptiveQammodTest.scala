package Chainsaw.comm

import Chainsaw._
import Chainsaw.comm.qam.AdaptiveQammod
import Chainsaw.dspTest._
import breeze.linalg._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

import scala.math.sqrt

class AdaptiveQammodTest extends AnyFunSuite {
  // TODO: design tests for more different situations
  test("test qammod without bitAlloc and powAlloc") {

    val testSize = 10

    val dataType = FTN.symbolType

    val params = FTN.FtnParams(3, 226, true)
    val bitAlloc = params.channelInfo.bitAlloc
    val powAlloc = params.channelInfo.powAlloc

    val data = (0 until testSize).map(_ => bitAlloc.map(bit => ChainsawRand.nextInt(1 << bit)))

    val testCases = data.map { int =>
      val string = int.zip(bitAlloc).map { case (bit, size) =>
        if (size == 0) "" else bit.toBinaryString.padToLeft(size, '0')
      }.mkString("")
      BigInt(string, 2)
    }

    val goldens = data.map(d => d.zip(bitAlloc.zip(powAlloc)).map { case (bit, (size, pow)) =>
      if (size == 0) BComplex(0.0, 0.0)
      else algos.Qam.qammod(DenseVector(bit), 1 << size)(0) * BComplex(sqrt(pow), 0.0)
    }.toSeq)

    doFlowPeekPokeTest(
      "testAdaptiveQAM", AdaptiveQammod(bitAlloc, powAlloc, dataType),
      testCases, goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-2)
  }
}
