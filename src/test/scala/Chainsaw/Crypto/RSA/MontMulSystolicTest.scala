package Chainsaw.Crypto.RSA

import Chainsaw.{toWords, _}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimConfig, _}

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil

class MontMulSystolicTest extends AnyFunSuite {

  test("testMontMulHardware") {
    //    GenRTL(new MontMulSystolic(8, 4, 8))
    // simulation: for n,w,p = 8,4,3, p == e
    def sim() = {
      val testSize = 512
      val testWordSize = 32
      val testPENumber = ceil((testSize + 1).toDouble / testWordSize).toInt // number of words

      SimConfig.withWave.compile(new MontMulSystolic(testSize, testWordSize, testPENumber)).doSim { dut =>
        import dut._
        val round = ceil(n.toDouble / p).toInt
        println(s"round = $round")
        clockDomain.forkStimulus(2)
        io.start #= false
        clockDomain.waitSampling()

        def push(X: BigInt, Y: BigInt, M: BigInt) = {
          println(s"X: $X Y: $Y M: $M")
          val XBits = X.toString(2).padToLeft(w * e, '0').reverse.map(_.asDigit)
          val YWords = toWords(Y, w, e)
          val MWords = toWords(M, w, e)
          val dutResults = ArrayBuffer[BigInt]()
          println(s"the output provider is ${outputProvider}")
          println(s"the queue depth is ${QueueDepth}")
          clockDomain.waitSampling(5)

          io.start #= true
          clockDomain.waitSampling()
          io.start #= false
          (0 until round).foreach { r =>
            (0 until e).foreach { i =>
              if (r == 0) {
                io.YWordIn #= YWords(i)
                io.MWordIn #= MWords(i)
              }
              if (i < p) io.xiIn #= XBits(r * p + i)
              if (io.validOut.toBoolean) dutResults += io.SWordOut.toBigInt
              clockDomain.waitSampling()
            }
          }
          (0 until e).foreach { _ =>
            if (io.validOut.toBoolean) dutResults += io.SWordOut.toBigInt
            clockDomain.waitSampling()
          }
          val golden = MontAlgos.Arch1MM(X, Y, M, w, print = true)
          println(s"Yours:  ${dutResults.map(_.toString(16).padToLeft(w / 4, '0')).mkString(" ")}")
          val dutResultBinary = dutResults.take(e).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString("")
          val yours = BigInt(dutResultBinary, 2) >> 1
          assertResult(golden)(yours)
        }
        if (Array(512, 1024, 2048, 3072, 4096).contains(testSize)) {
          def randRSASim() = {
            val ref = new RSARef(testSize)
            push(ref.getPrivateValue, ref.getPrivateValue, ref.getModulus)
          }
          randRSASim()
          randRSASim()
          randRSASim()
        } else {
          def randBigInt = BigInt("1" + (0 until lN - 2).map(_ => DSPRand.nextInt(2)).mkString("") + "1", 2)
          push(randBigInt, randBigInt, randBigInt)
          push(randBigInt, randBigInt, randBigInt)
          push(randBigInt, randBigInt, randBigInt)
        }
      }
    }
    sim()
    //    VivadoSynth(new MontMulPE(32))
  }

}
