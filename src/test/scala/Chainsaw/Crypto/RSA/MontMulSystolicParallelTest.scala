package Chainsaw.Crypto.RSA

import Chainsaw.{toWords, _}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimConfig, _}

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil

class MontMulSystolicParallelTest extends AnyFunSuite {

  test("testMontMulParallelHardware") {
    //    GenRTL(new MontMulSystolic(8, 4, 8))
    // simulation: for n,w,p = 8,4,3, p == e
    def sim() = {
      val testSizes = Seq(512, 1024, 2048, 3072, 4096)
      val testWordSize = 32
      val testPENumber = ceil((testSizes.min + 1).toDouble / testWordSize).toInt - 1 // number of words

      SimConfig.withWave.compile(new MontMulSystolicParallel(MontConfig(testSizes, testWordSize, testPENumber, parallel = true))).doSim { dut =>
        import dut._
        import config._
        clockDomain.forkStimulus(2)
        io.start #= false
        clockDomain.waitSampling()

        def push(X: BigInt, Y: BigInt, M: BigInt, channel: Int = 0) = {
          //          println(s"X: $X Y: $Y M: $M")
          val mode = testSizes.indexOf(M.bitLength)
          val groupPerInstance = M.bitLength / lMs.min
          val portId = channel * groupPerInstance
          val XBits = X.toString(2).padToLeft(es(mode) * rounds(mode), '0').reverse.map(_.asDigit)
          val YWords = toWords(Y, w, es(mode))
          val MWords = toWords(M, w, es(mode))
          val dutResults = ArrayBuffer[BigInt]()
          println(s"portId is $portId")
          println(s"the output provider is ${outputProviders(portId)}")
          clockDomain.waitSampling(5)

          val currentP = lMs(mode) / lMs.min * (p / parallelFactor)
          println(s"currentP = $currentP")

          io.start #= true
          io.mode #= BigInt(1) << mode
          clockDomain.waitSampling()
          io.start #= false
          io.mode #= BigInt(0)
          (0 until rounds(mode)).foreach { r =>

            (0 until es(mode)).foreach { i =>
              if (r == 0) {
                io.YWordIns(portId) #= YWords(i)
                io.MWordIns(portId) #= MWords(i)
              }
              if (i < currentP) io.xiIns(portId) #= XBits(r * currentP + i)
              if (io.valids(portId).toBoolean) dutResults += io.dataOuts(portId).toBigInt
              clockDomain.waitSampling()
            }
          }
          (0 until es(mode)).foreach { _ =>
            if (io.valids(portId).toBoolean) dutResults += io.dataOuts(portId).toBigInt
            clockDomain.waitSampling()
          }
          val golden = MontAlgos.Arch1MM(X, Y, M, w, print = true)
          println(s"Yours:  ${dutResults.map(_.toString(16).padToLeft(w / 4, '0')).mkString(" ")}")
          val dutResultBinary = dutResults.take(es(mode)).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString("")
          val dutResultHex = dutResults.take(es(mode)).reverse.map(_.toString(16).padToLeft(w/4, '0')).flatten.mkString("")
          println(s"result binary $dutResultBinary")
          println(s"result hex $dutResultHex")
          val yours = BigInt("0" + dutResultBinary, 2) >> 1
          assertResult(golden)(yours)
        }

        def randRSASim(lN: Int = 512, channel:Int = 0) = {
          val ref = new RSARef(lN)
          push(ref.getPrivateValue, ref.getPrivateValue, ref.getModulus)
        }
        randRSASim(512,0)
        randRSASim(512,1)
        randRSASim(512,2)
        randRSASim(512,3)
        randRSASim(1024, 0)
        randRSASim(1024, 1)
        randRSASim(2048, 0)
        randRSASim(2048, 1)
        //        randRSASim(3072)
        //        randRSASim(4096)
      }
    }
    sim()
  }

}