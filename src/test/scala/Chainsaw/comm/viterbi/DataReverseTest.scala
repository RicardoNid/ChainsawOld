package Chainsaw.comm.viterbi

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

class DataReverseTest extends AnyFlatSpec {

  behavior of "DataReverseTest"

  val testSize = 10
  val testLength = 128
  val testCase = (0 until testSize * testLength).map(_ => ChainsawRand.nextBigInt(8))
  val golden = testCase.grouped(testLength).toSeq.flatMap(_.reverse)

  it should "apply for readAsync" in doFlowPeekPokeTest("testDataReverse", DataReverse(UInt(64 bits), 128), testCase, golden)

  it should "apply for readSync" in doFlowPeekPokeTest("testDataReverse", DataReverse(UInt(64 bits), 128, readAsync = false), testCase, golden)

  it should "synth for readASync" in VivadoSynth( DataReverse(UInt(64 bits), 128), "DataReverseAsync")

  it should "synth for readSync" in VivadoSynth( DataReverse(UInt(64 bits), 128, readAsync = false), "DataReverseSync")

}
