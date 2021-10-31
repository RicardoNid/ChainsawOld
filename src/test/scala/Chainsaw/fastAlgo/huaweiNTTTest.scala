package Chainsaw.fastAlgo

import Chainsaw._
import Chainsaw.dspTest.doFlowPeekPokeTest
import org.scalatest.flatspec.AnyFlatSpec
import cc.redberry.rings
import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._

import scala.collection.mutable.ArrayBuffer

class huaweiNTTTest extends AnyFlatSpec {

  val cfRing = Zp(3329)

  "K2RED" should "pass the random test" in {
    val testCases = (0 until 100 * 100).map(_ => BigInt(DSPRand.nextInt(4095 * 4095))) // 10000-point random test
    doFlowPeekPokeTest(K2REDHard(), "testK2RED", testCases, testCases.map(c => lattice.K2RED(c.toInt, 3329)))
  }

  it should "pass the exhaustive test" in {
    val testCases = (0 until 4095 * 4095).map(BigInt(_)) // exhaustive test
    doFlowPeekPokeTest(K2REDHard(), "testK2RED", testCases, testCases.map(c => lattice.K2RED(c.toInt, 3329)))
  }

  it should "synth correctly" in VivadoSynth(K2REDHard()) // 75LUT + 31FF

  "KMultMod" should "pass the random test" in{
    val testCases = (0 until 100 * 100).map(_ => BigInt(DSPRand.nextInt(4095))).grouped(2).toSeq // 5000-pair random test
    val golden = testCases.map(vec => (vec(0) * vec(1) * 13 * 13) % 3329).map(cfRing(_))
    val dutResults = doFlowPeekPokeTest(KMultModHard(), "testKMultMod", testCases, null).asInstanceOf[ArrayBuffer[BigInt]].map(cfRing(_))
    assert(dutResults.diff(golden).isEmpty) // they're the same on the cfRing
  }

  it should "synth correctly" in VivadoSynth(KMultModHard(), name = "kMultMod") // 75LUT + 31FF + 1DSP / 238 LUT + 44 FF

}
