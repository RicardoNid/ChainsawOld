package Chainsaw.algos

import Chainsaw.algos.Dct._
import breeze.linalg.DenseVector
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class DctTest extends AnyFlatSpec {

  val testCases = (1 until 10).map(i => DenseVector.rand[Double](1 << i))

  behavior of "DctTest"

  it should "idct1D and dct1D" in testCases.foreach(testCase => assert(idct1D(dct1D(testCase)) ~= testCase))

  it should "dct1DByDft" in {
    testCases.foreach(testCase => assert(idct1D(dct1DByDft(testCase, 0)) ~= testCase))
    testCases.foreach(testCase => assert(idct1D(dct1DByDft(testCase, 1)) ~= testCase))
  }

  it should "dct1ByDoc" in {
    testCases.foreach { testCase =>
      assert(dct1DByDoc(testCase) ~= dct1D(testCase))
      logger.info(s"test on ${testCase.length}-point passed")
    }
  }
}
