package Chainsaw.Crypto.RSA

import Chainsaw._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil

class MontExpSystolicTest extends AnyFunSuite {

  test("testMontExpSystolicHardwareWithROM") {

    // design parameters that are determined by the user
    val testSizes = Seq(512, 1024, 2048)
    val testWordSize = 32
    
    val ref = new RSARef(512)

    val M = BigInt(ref.getModulus)
    println(s"length of the modulus: ${M.bitLength}")
    // you can use the public key for a faster test
    val E = BigInt(ref.getPrivateValue)
    val ELength = E.bitLength

    val Xs = (0 until 8).map(_ => BigInt(ref.getPrivateValue) / DSPRand.nextInt(10000) - DSPRand.nextInt(10000))
    //    val Xs = (0 until 8).map(_ => BigInt(DSPRand.nextInt(1000000)))

    import cc.redberry.rings.scaladsl._
    val r = BigInt(1) << (M.bitLength + 2)
    val rSquare = BigInt(Zp(M)(r * r).toByteArray)
    //    GenRTL(new MontExpSystolic(MontConfig(parallel = true), rSquare, M, E, ELength, Xs))

    val goldens = Xs.map(MontAlgos.Arch1MM(rSquare, _, M, testWordSize, print = true))
    printlnGreen(s"goldens >= M exists: ${goldens.exists(_ >= M)}")

    println("X0     : " + toWordsHexString(Xs(0), testWordSize, 16))
    println("M      : " + toWordsHexString(M, testWordSize, 16))
    println("rSquare: " + toWordsHexString(rSquare, testWordSize, 16))

    val dutResults = Seq.fill(8)(ArrayBuffer[BigInt]())

    MontAlgos.Arch1ME(Xs(0), E, M, testWordSize, true)

    SimConfig.withWave.compile(new MontExpSystolic(MontConfig(lMs = testSizes, parallel = true), rSquare, M, E, ELength, Xs)).doSim { dut =>
      import dut._

      val modeId = 0

      io.start #= false
      io.mode #= BigInt(1) << modeId
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()

      io.start #= true
      io.mode #= BigInt(1) << 0
      clockDomain.waitSampling()
      io.start #= false
      io.mode #= BigInt(0)

      val runtime = config.IIs(modeId) * dut.E.toString(2).map(_.asDigit + 1).sum + 200

      // monitors
      def montMulResultMonitor() = if (io.valids(0).toBoolean) dutResults.zip(io.dataOuts).foreach { case (buffer, signal) => buffer += signal.toBigInt }
      def montExpResultMonitor() = if()
      (0 until runtime).foreach { _ =>
        //          monitorResult()
        clockDomain.waitSampling()
      }
      goldens.indices.foreach { i =>
        val goldenStringBeforeShift = toWordsHexString(goldens(i), testWordSize, 16 + 1)
        val goldenString = toWordsHexString(goldens(i) << 1, testWordSize, 16 + 1)
        val dutString = dutResults(i).map(_.toString(16).padToLeft(32 / 4, '0')).mkString(" ") + " "

        //          println(s"golden result$i        : $goldenStringBeforeShift")
        //          println(s"golden result shifted$i: $goldenString")
        //          println(s"dut result shifted$i   : $dutString")
        //          assertResult(goldenString)(dutString)
      }
    }
  }
}