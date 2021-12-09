package Chainsaw.fastAlgos

import Chainsaw.fastAlgos.Dct._
import breeze.linalg.DenseVector
import org.scalatest.flatspec.AnyFlatSpec

class DctTest extends AnyFlatSpec {

  val testCases = (0 until 100).map(_ => DenseVector.rand[Double](100))

  behavior of "DctTest"

  it should "idct1D and dct1D" in testCases.foreach(testCase => assert(idct1D(dct1D(testCase)) ~= testCase))

  it should "dct1DByDft" in {
    testCases.foreach(testCase => assert(idct1D(dct1DByDft(testCase, 0)) ~= testCase))
    testCases.foreach(testCase => assert(idct1D(dct1DByDft(testCase, 1)) ~= testCase))
  }
}
