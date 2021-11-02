package Chainsaw.fastAlgo

import Chainsaw._
import Chainsaw.dspTest._
import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import Chainsaw.crypto._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class huaweiNTTTest extends AnyFlatSpec {

  val p = 3329
  implicit val polyRing = UnivariateRingZp64(p, "x")
  implicit val cfRing = polyRing.cfRing

  val k2 = 13 * 13
  val k2Inverse = cfRing.pow(k2, -1)

  import Chainsaw.DFG._
  // TODO: test method should support int/long as a replacement of bigint

  val testProducts: Seq[BigInt] = (0 until 10000).map(_ => BigInt(DSPRand.nextInt((p - 1) * (p - 1) + 1))) // 10000-point random test
  val testProductsExhaustive: Seq[BigInt] = (0 to (p - 1) * (p - 1)).map(BigInt(_))
  val testPairs: Seq[Seq[BigInt]] = (0 until 10000).map(_ => BigInt(DSPRand.nextInt(p))).grouped(2).toSeq

  "K2RED" should "pass the random test" in {
    val golden = testProducts.map(c => crypto.ModularReduction.K2RED(c.toInt, cfRing))
    doFlowPeekPokeTest("testK2RED", K2REDHard(), testProducts, golden)
  }

  "KMultMod" should "pass the random test" in {
    val golden = testPairs.map(vec => (vec(0) * vec(1) * k2) % p).map(cfRing(_))
    testDSPNode(huaweiNTT.kMultModNode, Seq(12 bits, 12 bits), testPairs, golden)
  }

  "KAddMod" should "pass the random test" in {
    val golden = testPairs.map(pair => cfRing(pair(0) + pair(1)))
    testDSPNode(BinaryNode(huaweiNTT.kAddMod, "kAddMod", delay = 1 cycles), Seq(12 bits, 12 bits), testPairs, golden)
  }

  "KSubMod" should "pass the random test" in {
    val golden = testPairs.map(pair => cfRing(pair(0) - pair(1)))
    testDSPNode(BinaryNode(huaweiNTT.kSubMod, "kSubMod", delay = 1 cycles), Seq(12 bits, 12 bits), testPairs, golden)
  }

  "CTBF" should "pass the random test" in {
    val omega = DSPRand.nextInt(p)
    val golden = testPairs.map { vec =>
      val (u, v) = (vec(0), vec(1))
      val vw = cfRing(k2 * v * omega)
      (cfRing(u + vw), cfRing(u - vw))
    }.flatMap(pair => Seq(pair._1, pair._2))

    val dutResults = doFlowPeekPokeTest("testCTBF", CTBFHard(omega), testPairs, golden).asInstanceOf[ArrayBuffer[BigInt]].map(cfRing(_))
    assert(dutResults.forall(cfRing.isCorrect))
  }

  "GSBF" should "pass the random test" in {
    val omega = DSPRand.nextInt(p)
    val golden = testPairs.map { vec =>
      val (u, v) = (vec(0), vec(1))
      (cfRing(u + v), cfRing(k2 * (u - v) * omega))
    }.flatMap(pair => Seq(pair._1, pair._2))

    val dutResults = doFlowPeekPokeTest("testGSBF", GSBFHard(omega), testPairs, golden).asInstanceOf[ArrayBuffer[BigInt]].map(cfRing(_))
    assert(dutResults.forall(cfRing.isCorrect))
  }

  "all these operators" should "synth correctly" in {
//    VivadoSynth(K2REDHard(), name = "kRED")
    synthDSPNode(huaweiNTT.kMultModNode, Seq(12 bits, 12 bits))
//    VivadoSynth(CTBFHard(omega = DSPRand.nextInt(p)), name = "ctbf")
//    VivadoSynth(GSBFHard(omega = DSPRand.nextInt(p)), name = "gsbf")
  }

}
