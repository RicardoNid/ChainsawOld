package Chainsaw.crypto.RSA

import Chainsaw._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimConfig, _}

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil

class MontMultSystolicTest extends AnyFunSuite {

  // FIXME: fix this after 11.15

//  test("testMontMulParallelHardware") {
//    //    GenRTL(new MontMulSystolic(8, 4, 8))
//    // simulation: for n,w,p = 8,4,3, p == e
//    def sim() = {
//      val testSizes = Seq(512, 1024, 2048, 3072, 4096)
//      val testWordSize = 32
//      val testPENumber = ceil((testSizes.min + 1).toDouble / testWordSize).toInt - 1 // number of words
//
//      SimConfig.withWave.compile(new MontMultSystolic(MontConfig(testSizes, testWordSize, testPENumber, parallel = true))).doSim { dut =>
//        import dut._
//        import config._
//        clockDomain.forkStimulus(2)
//        clockDomain.waitSampling()
//
//        def push(X: BigInt, Y: BigInt, M: BigInt, channel: Int = 0) = {
//          //          println(s"X: $X Y: $Y M: $M")
//          val mode = testSizes.indexOf(M.bitLength)
//          val groupPerInstance = M.bitLength / lMs.min
//          println(s"groupPerInstance $groupPerInstance")
//          val portId = channel * groupPerInstance
//          val XBits = X.toString(2).padToLeft(es(mode) * rounds(mode), '0').reverse.map(_.asDigit)
//          val YWords = toWords(Y, w, es(mode))
//          val MWords = toWords(M, w, es(mode))
//          val dutResults = ArrayBuffer[BigInt]()
//          println(s"portId is $portId")
//          println(s"the output provider is ${outputProviders(portId)}")
//          clockDomain.waitSampling(5)
//
//          val currentP = lMs(mode) / lMs.min * (p / parallelFactor)
//          println(s"currentP = $currentP")
//
//          io.mode #= BigInt(1) << mode
//          clockDomain.waitSampling()
//
//          //          io.mode #= BigInt(0)
//          (0 until rounds(mode)).foreach { r =>
//
//            (0 until es(mode)).foreach { i =>
//              if (r == 0) {
//                io.YWordIns(portId) #= YWords(i)
//                io.MWordIns(portId) #= MWords(i)
//              }
//              if (i < currentP) io.xiIns(portId) #= XBits(r * currentP + i)
//              //              if (io.valids(portId).toBoolean) dutResults += io.dataOuts(portId).toBigInt
//              clockDomain.waitSampling()
//            }
//          }
//          (0 until es(mode)).foreach { _ =>
//            //            if (io.valids(portId).toBoolean) dutResults += io.dataOuts(portId).toBigInt
//            clockDomain.waitSampling()
//          }
//          val golden = MontAlgos.Arch1MM(X, Y, M, w, print = true)
//          println(s"Yours:  ${dutResults.map(_.toString(16).padToLeft(w / 4, '0')).mkString(" ")}")
//          val dutResultBinary = dutResults.take(es(mode)).reverse.map(_.toString(2).padToLeft(w, '0')).flatten.mkString("")
//          val dutResultHex = dutResults.take(es(mode)).reverse.map(_.toString(16).padToLeft(w / 4, '0')).flatten.mkString("")
//
//          //          println(s"result binary $dutResultBinary")
//          //          println(s"result hex $dutResultHex")
//          // drop the first bit which is generated by a different Xi, but actually don't care, as we've padded X
//          // we take e = ceil(n + 2 / w) rather than n + 1 to make it really don't care
//          // the reason is there's a bypass from the previous PE, and that PE would be influenced by the cut off of setXi in buffer
//          val yours = BigInt(dutResultBinary.tail, 2) >> 1
//          assertResult(golden)(yours)
//        }
//
//        def randRSASim(lN: Int = 512, channel: Int = 0) = {
//          val ref = new Refs(lN)
//          push(ref.getPrivateValue, ref.getPrivateValue, ref.getModulus, channel)
//        }
//        (0 until 8).foreach(randRSASim(512, _)) // 8 in parallel
//        (0 until 4).foreach(randRSASim(1024, _))
//        (0 until 2).foreach(randRSASim(2048, _)) // 2 in parallel
//        //                (0 until 1).foreach(randRSASim(3072, _))
//        //                (0 until 1).foreach(randRSASim(4096, _))
//      }
//    }
//    sim()
//  }

}