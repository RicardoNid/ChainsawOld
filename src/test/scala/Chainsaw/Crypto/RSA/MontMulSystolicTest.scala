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
      val testSizes = Seq(512, 1024, 2048)
      val testWordSize = 32
      val testPENumber = ceil((testSizes.min + 1).toDouble / testWordSize).toInt // number of words

      SimConfig.withWave.compile(new MontMulSystolic(testSizes, testWordSize, testPENumber)).doSim { dut =>
        import dut._
        clockDomain.forkStimulus(2)
        io.start #= false
        clockDomain.waitSampling()

        def push(X: BigInt, Y: BigInt, M: BigInt, mode: Int = 0) = {
          println(s"X: $X Y: $Y M: $M")
          val XBits = X.toString(2).padToLeft(w * es(mode), '0').reverse.map(_.asDigit)
          val YWords = toWords(Y, w, es(mode))
          val MWords = toWords(M, w, es(mode))
          val dutResults = ArrayBuffer[BigInt]()
          println(s"the output provider is ${outputProviders(mode)}")
          println(s"the queue depth is ${QueueDepths(mode)}")
          clockDomain.waitSampling(5)

          io.start #= true
          clockDomain.waitSampling()
          io.start #= false
          (0 until rounds(mode)).foreach { r =>

            io.mode #= BigInt(1) << mode

            (0 until es(mode)).foreach { i =>
              if (r == 0) {
                io.YWordIn #= YWords(i)
                io.MWordIn #= MWords(i)
              }
              if (i < p) io.xiIn #= XBits(r * p + i)
              if (io.valid.toBoolean) dutResults += io.dataOut.toBigInt
              clockDomain.waitSampling()
            }
          }
          (0 until es(mode)).foreach { _ =>
            if (io.valid.toBoolean) dutResults += io.dataOut.toBigInt
            clockDomain.waitSampling()
          }
          val golden = MontAlgos.Arch1MM(X, Y, M, w, print = true)
          println(s"Yours:  ${dutResults.map(_.toString(16).padToLeft(w / 4, '0')).mkString(" ")}")
          val dutResultBinary = dutResults.take(es(mode)).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString("")
          val yours = BigInt("0" + dutResultBinary, 2) >> 1
          assertResult(golden)(yours)
        }

        def randRSASim() = {
          val ref = new RSARef(512)
          push(ref.getPrivateValue, ref.getPrivateValue, ref.getModulus, 0)
        }
        randRSASim()
        randRSASim()
        randRSASim()

        //        if (Array(512, 1024, 2048, 3072, 4096).contains(testSize)) {
        //
        //
        //        } else {
        //          def randBigInt = BigInt("1" + (0 until lNs - 2).map(_ => DSPRand.nextInt(2)).mkString("") + "1", 2)
        //          push(randBigInt, randBigInt, randBigInt)
        //          push(randBigInt, randBigInt, randBigInt)
        //          push(randBigInt, randBigInt, randBigInt)
        //        }
      }
    }
    sim()
    //    VivadoSynth(new MontMulPE(32))
  }

}
