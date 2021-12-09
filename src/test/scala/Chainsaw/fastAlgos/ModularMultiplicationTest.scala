package Chainsaw.fastAlgos

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import org.scalatest.flatspec.AnyFlatSpec

class ModularMultiplicationTest extends AnyFlatSpec {

  // preparing RSA params for test
  val lN = 1024
  val testCount = 1000
  val rsaParams = RsaParams(lN)
  val (ns, publicKeys, privateKeys) = rsaParams.getParams(testCount)

  val as = ns.map(n => ChainsawRand.nextBigInt(lN) % n)
  val bs = ns.map(n => ChainsawRand.nextBigInt(lN) % n)

  "Montgomery modular multiplication" should "be work" in {
    (0 until testCount).foreach(i => ModularMultiplication.mmm(as(i), bs(i), ns(i)))
  }

  "McLaughlin Montgomery modular multiplication" should "be work" in {
    (0 until testCount).foreach(i => ModularMultiplication.mlm(as(i), bs(i), ns(i)))
  }

  "McLaughlin multiplication without conditional selections" should "be work" in {
    (0 until testCount).foreach(i => ModularMultiplication.mlws(as(i), bs(i), ns(i)))
  }
}
