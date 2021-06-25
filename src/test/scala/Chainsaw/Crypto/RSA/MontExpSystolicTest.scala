package Chainsaw.Crypto.RSA

import Chainsaw._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import cc.redberry.rings.scaladsl._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil

class MontExpSystolicTest extends AnyFunSuite {

  test("testMontExpSystolicHardwareWithROM") {

    // design parameters that are determined by the user
    val testSizes = Seq(512, 1024, 2048, 3072, 4096)
    val testWordSize = 32

    // generate stimulus for this testCase
    val ref = new RSARef(512)
    val testModulus = BigInt(ref.getModulus)
    println(s"length of the modulus: ${testModulus.bitLength}")
    // you can use the public key for a faster test
    val testExponent = BigInt(ref.getPublicValue)
    println(s"exponent = ${testExponent.toString(2)}")
    val testRadix = BigInt(1) << (testModulus.bitLength + 2)
    val testRadixSquare = BigInt(Zp(testModulus)(testRadix * testRadix).toByteArray)
    val testExponentLength = testExponent.bitLength

    val testExponentWords = toWords(BigInt(testExponent.toString(2).reverse, 2), testWordSize, 512 / testWordSize)
    val testRadixSquareWords = toWords(testRadixSquare, testWordSize, 512 / testWordSize)
    val testModulusWords = toWords(testModulus, testWordSize, 512 / testWordSize)

    val testInputs = (0 until 8).map(_ => BigInt(ref.getPrivateValue) / DSPRand.nextInt(10000) - DSPRand.nextInt(10000))
    val testInputsWords = testInputs.map(input => toWords(input, testWordSize, 512 / testWordSize))

    val goldens = testInputs.map(MontAlgos.Arch1ME(_, testExponent, testModulus, testWordSize, print = false))
    printlnGreen(s"goldens >= M exists: ${goldens.exists(_ >= testModulus)}")

    GenRTL(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true), testRadixSquare, testModulus, testExponent, testExponentLength, testInputs))

    SimConfig.withWave.compile(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true), testRadixSquare, testModulus, testExponent, testExponentLength, testInputs)).doSim { dut =>
      import dut._
      import dut.config._

      val modeId = 0

      io.start #= false
      io.mode #= BigInt(0) << modeId
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()

      def startAMontExp(): Unit = {
        io.start #= true
        io.mode #= BigInt(1) << modeId
        io.exponentLengthIn #= testExponentLength
        io.keyReset #= true
        clockDomain.waitSampling()
      }

      def unsetStart() = {
        io.start #= false
        io.mode #= BigInt(0)
        io.exponentLengthIn #= 0
        io.keyReset #= false
      }

      def runForOnce(modeId: Int) = {
        val dutResults = Seq.fill(8)(ArrayBuffer[BigInt]())
        // monitors
        def montMulResultMonitor() = if (montMult.io.valids(0).toBoolean) dutResults.zip(io.dataOuts).foreach { case (buffer, signal) => buffer += signal.toBigInt }
        def montExpResultMonitor() = if (io.valids(0).toBoolean) dutResults.zip(io.dataOuts).foreach { case (buffer, signal) => buffer += signal.toBigInt }

        val runtime = config.IIs(modeId) * dut.E.toString(2).map(_.asDigit + 1).sum + 200
        val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(modeId) == 0)

        startAMontExp()

        (0 until wordPerInstance(modeId)).foreach { i => // feed
          if(i == 0) unsetStart()
          io.modulusWordIn #= testModulusWords(i)
          io.radixSquareWordIn #= testRadixSquareWords(i)
          io.exponentWordIn #= testExponentWords(i)
          starterIds.zipWithIndex.foreach { case (starter, inputId) => io.xWordIns(starter) #= testInputsWords(inputId)(i) }
          clockDomain.waitSampling()
        }

        (0 until runtime).foreach { _ =>
          montExpResultMonitor()
          clockDomain.waitSampling()
        }

        // output
        printlnYellow(s"test of mode $modeId, which run ${parallelFactor / groupPerInstance(modeId)} instance of size ${lMs(modeId)}")
        println("X0     : " + toWordsHexString(testInputs(0), testWordSize, 16))
        println("M      : " + toWordsHexString(testModulus, testWordSize, 16))
        println("rSquare: " + toWordsHexString(testRadixSquare, testWordSize, 16))
        goldens.indices.foreach { i =>
          val goldenString = toWordsHexString(goldens(i), testWordSize, lMs(modeId) / w)
          val dutString = dutResults(i).init.map(_.toString(16).padToLeft(32 / 4, '0')).mkString(" ") + " "
          println(s"golden result$i        : $goldenString")
          println(s"dut result$i           : $dutString")
          assertResult(goldenString)(dutString)
        }
      }

      runForOnce(0)
    }

    //    VivadoSynth(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true), testRadixSquare, testModulus, testExponent, testExponentLength, testInputs))
  }
}