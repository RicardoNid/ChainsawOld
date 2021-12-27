package Chainsaw.comm

import Chainsaw.comm.qam.{AdaptiveQammod, Refs}
import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.math.sqrt

class AdaptiveQammodTest extends AnyFunSuite {
  // TODO: design tests for more different situations
  test("test qammod without bitAlloc and powAlloc") {

    val testSize = 10
    val parallelism = 256

    val dataType = HardType(SFix(1 exp, -14 exp))
    val bitAlloc = Seq.fill(parallelism / 2)(2) ++ Seq.fill(parallelism / 2)(6)
    val powAlloc = DenseVector.rand[Double](parallelism).toSeq

    val data = (0 until testSize).map(_ => bitAlloc.map(bit => ChainsawRand.nextInt(1 << bit)))
    println(data.mkString(" "))

    val testCases = data.map(d =>
      BigInt(d.zip(bitAlloc).map { case (bit, size) =>
        bit.toBinaryString.padToLeft(size, '0')
      }.mkString(""), 2))

    val goldens = data.map(d => d.zip(bitAlloc.zip(powAlloc)).map { case (bit, (size, pow)) =>
      algos.Qam.qammod(DenseVector(bit), 1 << size)(0) * BComplex(sqrt(pow), 0.0)
    })

    doFlowPeekPokeTest(
      "testAdaptiveQAM", AdaptiveQammod(bitAlloc, powAlloc, dataType),
      testCases, goldens,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-4)
  }
}
