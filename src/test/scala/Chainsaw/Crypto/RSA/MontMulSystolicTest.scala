//package Chainsaw.Crypto.RSA
//
//import Chainsaw.{toWords, _}
//import org.scalatest.funsuite.AnyFunSuite
//import spinal.core.sim.{SimConfig, _}
//
//import scala.collection.mutable.ArrayBuffer
//import scala.math.ceil
//
//class MontMulSystolicTest extends AnyFunSuite {
//
//  test("testMontMulHardware") {
//    //    GenRTL(new MontMulSystolic(8, 4, 8))
//    // simulation: for n,w,p = 8,4,3, p == e
//    def sim() = {
//      val testSizes = Seq(512, 1024, 2048, 3072, 4096)
//      val testWordSize = 32
//      val testPENumber = ceil((testSizes.min + 1).toDouble / testWordSize).toInt - 1 // number of words
//
//      SimConfig.withWave.compile(new MontMulSystolic(MontConfig(testSizes, testWordSize, testPENumber))).doSim { dut =>
//        import dut._
//        import config._
//        clockDomain.forkStimulus(2)
//        io.start #= false
//        clockDomain.waitSampling()
//
//        def push(X: BigInt, Y: BigInt, M: BigInt) = {
//          //          println(s"X: $X Y: $Y M: $M")
//          val mode = testSizes.indexOf(M.bitLength)
//          val XBits = X.toString(2).padToLeft(w * es(mode), '0').reverse.map(_.asDigit)
//          val YWords = toWords(Y, w, es(mode))
//          val MWords = toWords(M, w, es(mode))
//          val dutResults = ArrayBuffer[BigInt]()
//          println(s"the output provider is ${outputProviders(mode)}")
//          println(s"the queue depth is ${queueDepths(mode)}")
//          clockDomain.waitSampling(5)
//
//          io.start #= true
//          io.mode #= BigInt(1) << mode
//          clockDomain.waitSampling()
//          io.start #= false
//          io.mode #= BigInt(0)
//          (0 until rounds(mode)).foreach { r =>
//
//            (0 until es(mode)).foreach { i =>
//              if (r == 0) {
//                io.YWordIn #= YWords(i)
//                io.MWordIn #= MWords(i)
//              }
//              if (i < p) io.xiIn #= XBits(r * p + i)
//              if (io.valid.toBoolean) dutResults += io.SWordOut.toBigInt
//              clockDomain.waitSampling()
//            }
//          }
//          (0 until es(mode)).foreach { _ =>
//            if (io.valid.toBoolean) dutResults += io.SWordOut.toBigInt
//            clockDomain.waitSampling()
//          }
//          val golden = MontAlgos.Arch1MM(X, Y, M, w, print = true)
//          println(s"Yours:  ${dutResults.map(_.toString(16).padToLeft(w / 4, '0')).mkString(" ")}")
//          val dutResultBinary = dutResults.take(es(mode)).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString("")
//          val yours = BigInt("0" + dutResultBinary, 2) >> 1
//          assertResult(golden)(yours)
//        }
//
//        def randRSASim(lN: Int = 512) = {
//          val ref = new RSARef(lN)
//          push(ref.getPrivateValue, ref.getPrivateValue, ref.getModulus)
//        }
//        randRSASim()
//        randRSASim(1024)
//
//        //        randRSASim(2048)
//        //        randRSASim(3072)
//        //        randRSASim(4096)
//      }
//    }
//    sim()
//  }
//
//}
