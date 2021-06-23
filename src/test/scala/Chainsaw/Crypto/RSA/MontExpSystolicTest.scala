package Chainsaw.Crypto.RSA

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

import scala.collection.mutable.ArrayBuffer

class MontExpSystolicTest extends AnyFunSuite {

  test("testMontExpSystolicHardwareWithROM") {
    val ref = new RSARef(512)

    val w = 32
    val M = BigInt(ref.getModulus)
    println(s"length of the modulus: ${M.bitLength}")
    val E = BigInt(ref.getPrivateValue)
    val ELength = E.bitLength

    val Xs = (0 until 8).map(_ => BigInt(ref.getPrivateValue) / DSPRand.nextInt(10000) - DSPRand.nextInt(10000))
    //    val Xs = (0 until 8).map(_ => BigInt(DSPRand.nextInt(1000000)))

    import cc.redberry.rings.scaladsl._
    val r = BigInt(1) << (M.bitLength + 2)
    val rSquare = BigInt(Zp(M)(r * r).toByteArray)
    //    GenRTL(new MontExpSystolic(MontConfig(parallel = true), rSquare, M, E, ELength, Xs))

    val goldens = Xs.map(MontAlgos.Arch1MM(rSquare, _, M, w, print = true))
    printlnGreen(s"goldens >= M exists: ${goldens.exists(_ >= M)}")

    println("X0     : " + toWordsHexString(Xs(0), w, 16))
    println("M      : " + toWordsHexString(M, w, 16))
    println("rSquare: " + toWordsHexString(rSquare, w, 16))

    val dutResults = Seq.fill(8)(ArrayBuffer[BigInt]())

    SimConfig.withWave.compile(new MontExpSystolic(MontConfig(parallel = true), rSquare, M, E, ELength, Xs)).doSim { dut =>
      import dut._

      io.start #= false
      io.mode #= BigInt(0)
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()

      io.start #= true
      io.mode #= BigInt(1) << 0
      clockDomain.waitSampling()
      io.start #= false
      io.mode #= BigInt(0)
      (0 until 2000).foreach { _ =>
        if (io.valids(0).toBoolean) {
          dutResults.zip(io.dataOuts).foreach { case (buffer, signal) => buffer += signal.toBigInt }
        }
//        if (checkRAM.toBoolean) {
//          println(toWordsHexString(productRAMs(0)(0).toBigInt, 32, 1))
//        }
        clockDomain.waitSampling()
      }
      goldens.indices.foreach { i =>
        val goldenStringBeforeShift = toWordsHexString(goldens(i), w, 16 + 1)
        val goldenString = toWordsHexString(goldens(i) << 1, w, 16 + 1)
        val dutString = dutResults(i).map(_.toString(16).padToLeft(32 / 4, '0')).mkString(" ") + " "
        println(s"golden result$i: $goldenStringBeforeShift")
        println(s"golden result$i: $goldenString")
        println(s"dut result$i   : $dutString")
        assertResult(goldenString)(dutString)
      }
    }
  }
}
