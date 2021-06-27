package Chainsaw.Crypto.RSA

import Chainsaw._
import cc.redberry.rings.scaladsl._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

case class MontExpTestCase(modeId: Int, changeKey: Boolean = true)

class MontExpSystolicTest extends AnyFunSuite {


  test("testMontExpSystolicHardwareWithROM") {

    val doGen = false
    val doSim = true
    val simTimes = 10
    val doSynth = false
    val doImpl = false
    val comparStageByStage = false

    val testCases = Seq(
      MontExpTestCase(0),
      MontExpTestCase(0, false),
      MontExpTestCase(0, false),
      MontExpTestCase(1),
      MontExpTestCase(1,false),
      MontExpTestCase(3, true),
      MontExpTestCase(4, true)
    )

    val testSizes = Seq(512, 1024, 2048, 3072, 4096)
    val testWordSize = 32
    if (doGen) GenRTL(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true))) // for a quick semantic test
    if (doSim) {
      def sim() = {
        SimConfig.withWave.compile(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true))).doSim { dut =>
          import dut._
          import dut.config._

          var lastModulus, lastRadixSquare, lastExponent = BigInt(0)

          def runTestCases(montExpTestCases: Seq[MontExpTestCase]) = {
            montExpTestCases.foreach { testcase =>
              // preparing data
              val modeId = testcase.modeId
              val currentTestSize = lMs(modeId)
              val currentInstanceNumber = instanceNumber(modeId)
              val currentWordPerInstance = wordPerInstance(modeId)

              val ref = new RSARef(currentTestSize)
              ref.refresh()
              val testModulus = if (testcase.changeKey) BigInt(ref.getModulus) else lastModulus

              val testRadix = BigInt(1) << (testModulus.bitLength + 2)
              val testRadixSquare = if (testcase.changeKey) BigInt(Zp(testModulus)(testRadix * testRadix).toByteArray) else lastRadixSquare
              //          val testExponent = if (testcase.changeKey) BigInt(ref.getPublicValue) else lastExponent
              val testExponent = BigInt("1001", 2)
              val testExponentLength = testExponent.bitLength

              lastModulus = testModulus
              lastRadixSquare = testRadixSquare
              lastExponent = testExponent
              println(s"exponent = ${testExponent.toString(2)}")

              val testInputs = (0 until currentInstanceNumber).map(_ => BigInt(ref.getPrivateValue) / DSPRand.nextInt(10000) - DSPRand.nextInt(10000))
              // get words
              val testExponentWords = toWords(BigInt(testExponent.toString(2).reverse, 2), testWordSize, currentTestSize / testWordSize)
              val testRadixSquareWords = toWords(testRadixSquare, testWordSize, currentTestSize / testWordSize)
              val testModulusWords = toWords(testModulus, testWordSize, currentTestSize / testWordSize)
              val testInputsWords = testInputs.map(input => toWords(input, testWordSize, currentTestSize / testWordSize))
              // get golden
              //          val goldens = testInputs.map(MontAlgos.Arch1ME(_, testExponent, testModulus, testWordSize, print = false))
              // FIXME: much faster, but may not be totally the same as results of Arch1ME
              val goldens = testInputs.map(input => BigInt(Zp(testModulus).pow(input, testExponent).toByteArray))
              printlnGreen(s"goldens >= M exists: ${goldens.exists(_ >= testModulus)}")

              def startAMontExp(): Unit = {
                io.start #= true
                io.mode #= BigInt(1) << testcase.modeId
                io.exponentLengthIn #= testExponentLength
                io.keyReset #= testcase.changeKey
                clockDomain.waitSampling()
              }

              def unsetStart() = {
                io.start #= false
                io.mode #= BigInt(0)
                io.exponentLengthIn #= 0
                io.keyReset #= false
              }

              def runForOnce() = {
                val dutResults = Seq.fill(currentInstanceNumber)(ArrayBuffer[BigInt]())
                // monitors
                val runtime = config.IIs(modeId) * testExponent.toString(2).map(_.asDigit + 1).sum + 200
                val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(modeId) == 0)
                  .take(parallelFactor / groupPerInstance(modeId))
                val currentDataOuts = io.dataOuts.indices.filter(starterIds.contains(_)).map(io.dataOuts(_))

                def montMulResultMonitor() = if (montMult.io.valids(0).toBoolean) dutResults.zip(io.dataOuts).foreach { case (buffer, signal) => buffer += signal.toBigInt }
                def montExpResultMonitor() = if (io.valids(0).toBoolean) dutResults.zip(currentDataOuts).foreach { case (buffer, signal) => buffer += signal.toBigInt }

                startAMontExp()

                (0 until wordPerInstance(modeId)).foreach { i => // STATE = INIT, feed
                  if (i == 0) unsetStart()
                  io.modulusWordIn #= testModulusWords(i)
                  io.radixSquareWordIn #= testRadixSquareWords(i)
                  io.exponentWordIn #= testExponentWords(i)
                  starterIds.zipWithIndex.foreach { case (starter, inputId) => io.xWordIns(starter) #= testInputsWords(inputId)(i) }
                  clockDomain.waitSampling()
                }

                (0 until runtime).foreach { _ => // STATE = RUN, run automatically, monitoring
                  montExpResultMonitor()
                  clockDomain.waitSampling()
                }

                // output
                printlnYellow(s"test of mode $modeId, which run ${parallelFactor / groupPerInstance(modeId)} instance of size ${lMs(modeId)}")
                testInputs.indices.foreach { i =>
                  println(s"X$i     : " + toWordsHexString(testInputs(i), testWordSize, currentWordPerInstance))
                }
                println("M      : " + toWordsHexString(testModulus, testWordSize, currentWordPerInstance))
                println("rSquare: " + toWordsHexString(testRadixSquare, testWordSize, currentWordPerInstance))
                if (comparStageByStage) MontAlgos.Arch1ME(testInputs(0), testExponent, testModulus, testWordSize, print = true) // print the partial products
                goldens.indices.foreach { i =>
                  val goldenString = toWordsHexString(goldens(i), testWordSize, lMs(modeId) / w)
                  val dutString = dutResults(i).init.map(_.toString(16).padToLeft(testWordSize / 4, '0')).mkString(" ") + " "
                  println(s"golden result$i        : $goldenString")
                  println(s"dut result$i           : $dutString")
                  assertResult(goldenString)(dutString)
                }
              }

              runForOnce()
            }
          }

          // main part
          io.start #= false
          io.mode #= BigInt(0)
          clockDomain.forkStimulus(2)
          clockDomain.waitSampling()

          runTestCases(testCases)
        }
      }
      (0 until simTimes).foreach(_ => sim())
    }
    //    if (doSynth) VivadoSynth(new MontMulPE(testWordSize))
    //    if (doSynth) VivadoSynth(new MontMulSystolicParallel(MontConfig(lMs = testSizes, parallel = true)))
    if (doSynth) VivadoSynth(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true)))
    if (doImpl) VivadoImpl(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true)))
  }
}