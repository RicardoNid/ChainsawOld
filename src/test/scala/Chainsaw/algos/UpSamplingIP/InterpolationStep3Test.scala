package Chainsaw.algos.UpSamplingIP

import spinal.lib._
import spinal.core._
import spinal.core.sim._
import org.slf4j._
import org.scalatest.funsuite._
import scala.collection.mutable._
import scala.util.Random._

object sim3Funcs {
  def startSim(threshold: Int, sH: Int, sW: Int, testCases: ArrayBuffer[Int], golden: ArrayBuffer[Int], isPrint: Boolean = false) = {
    val compiled   = SimConfig.withFstWave.compile(InterpolationStep3(IPConfig(sH, sW)))
    val results    = ArrayBuffer[Int]()
    val rowEndOuts = ArrayBuffer[Boolean]()
    val validOuts  = ArrayBuffer[Boolean]()
    var inCount    = -1
    compiled.doSimUntilVoid { dut =>
      val logger = LoggerFactory.getLogger(s"Test : InterpolationStep3")

      dut.io.dataOut.ready #= false
      dut.io.rowEndIn      #= false
      dut.io.frameStartIn  #= false
      dut.io.StartIn       #= false
      dut.io.widthIn       #= 1
      dut.io.heightIn      #= 1
      dut.io.inpValidIn    #= true

      val monitor = fork {
        while (true) {
          if (dut.io.dataOut.valid.toBoolean && dut.io.dataOut.ready.toBoolean) {
            results += dut.io.dataOut.payload.toInt
            rowEndOuts += dut.io.rowEndOut.toBoolean
            validOuts += dut.io.inpValidOut.toBoolean
          }
          sleep(2)
        }
      }

      dut.clockDomain.forkStimulus(2)
      val interpolation = fork {
        dut.clockDomain.waitSampling(5)
        dut.io.dataOut.ready #= true
        dut.io.widthIn       #= sW
        dut.io.heightIn      #= sH
        dut.io.thresholdIn   #= threshold
        dut.clockDomain.waitSampling()
        dut.io.StartIn #= true
        inCount = 0
        testCases.foreach { testCase =>
          dut.io.dataIn.payload #= testCase
          dut.io.dataIn.valid   #= true
          if (testCase == 0) {
            dut.io.inpValidIn #= false
          }else{
            dut.io.inpValidIn #= true
          }
          if (inCount == 0) {
            dut.io.frameStartIn #= true
          } else {
            dut.io.frameStartIn #= false
          }
          if ((inCount + 1) % (4 * sW) == 0) {
            dut.io.rowEndIn #= true
          } else {
            dut.io.rowEndIn #= false
          }
          dut.clockDomain.waitSamplingWhere(dut.io.dataIn.valid.toBoolean && dut.io.dataIn.ready.toBoolean)
          inCount += 1
          if (inCount == 4 * sW * sH) { dut.io.rowEndIn #= false }
        }

      }

      val getSimResult = fork {
        while (true) {
          if (results.length == 16 * sH * sW) {
            dut.io.StartIn #= false
            dut.clockDomain.waitSampling(2)
            if (isPrint) {
              val formatTestCases  = testCases.map(_.toString.padTo(5, ' ')).grouped(4 * sW).toSeq.map(_.mkString("")).mkString("\n")
              val formatGolden     = golden.map(_.toString.padTo(5, ' ')).grouped(4 * sW).toSeq.map(_.mkString("")).mkString("\n")
              val formatResults    = results.map(_.toString.padTo(5, ' ')).grouped(4 * sW).toSeq.map(_.mkString("")).mkString("\n")
              val formatRowEndOuts = rowEndOuts.map(_.toString.padTo(7, ' ')).grouped(4 * sW).toSeq.map(_.mkString("")).mkString("\n")
              logger.info(
                s"\n"
                  + "testCases : \n"
                  + formatTestCases
                  + s"\n"
                  + "golden : \n"
                  + formatGolden
                  + s"\n"
                  + s"results : \n"
                  + formatResults
                  + s"\n"
                  + s"rowEndOuts : \n"
                  + formatRowEndOuts
                  + s"\n"
              )
            }
            if (compareResults(results, golden)) {
              simSuccess()
            } else {
              simFailure("test fail !")
            }
          }
          //sleep(2)
          dut.clockDomain.waitSampling()
        }
      }

    }
  }

  def getGolden(threshold: Int, sH: Int, sW: Int, testCases: ArrayBuffer[Int], isPrint: Boolean = false) = {
    val golden = ArrayBuffer[Int]()
    val rows   = testCases.grouped(4 * sW).toBuffer

    Range(0, 4 * sH).foreach { rIdx =>
      Range(0, 4 * sW).foreach { cIdx =>
        val lastRow = rIdx match {
          case r if r == 0 => 1
          case r           => r - 1
        }
        val nextRow = rIdx match {
          case r if r == 4 * sH - 1 => 4 * sH - 2
          case r                    => r + 1
        }
        val leftIdx = cIdx match {
          case c if c == 0 => 1
          case c           => c - 1
        }
        val rightIdx = cIdx match {
          case c if c == 4 * sW - 1 => 4 * sW - 2
          case c                    => c + 1
        }
        if (rows(rIdx)(cIdx) == 0) {
          val candidate = ArrayBuffer[Tuple2[Int, Int]]()
          if (rows(rIdx)(leftIdx) != 0 && rows(rIdx)(rightIdx) != 0) {
            candidate += Tuple2((rows(rIdx)(leftIdx) - rows(rIdx)(rightIdx)).abs, (rows(rIdx)(leftIdx) + rows(rIdx)(rightIdx)) / 2)
          }

          if (rows(lastRow)(cIdx) != 0 && rows(nextRow)(cIdx) != 0) {
            candidate += Tuple2((rows(lastRow)(cIdx) - rows(nextRow)(cIdx)).abs, (rows(lastRow)(cIdx) + rows(nextRow)(cIdx)) / 2)
          }

          if (rows(lastRow)(leftIdx) != 0 && rows(nextRow)(rightIdx) != 0) {
            candidate += Tuple2((rows(lastRow)(leftIdx) - rows(nextRow)(rightIdx)).abs, (rows(lastRow)(leftIdx) + rows(nextRow)(rightIdx)) / 2)
          }

          if (rows(lastRow)(rightIdx) != 0 && rows(nextRow)(leftIdx) != 0) {
            candidate += Tuple2((rows(lastRow)(rightIdx) - rows(nextRow)(leftIdx)).abs, (rows(lastRow)(rightIdx) + rows(nextRow)(leftIdx)) / 2)
          }

          val sortedCandidate = candidate.sortBy(_._1)
          golden += sortedCandidate(0)._2

        } else {
          golden += rows(rIdx)(cIdx)
        }
      }
    }

//        println("***")
//        println(golden.map(_.toString.padTo(3, ' ')).grouped(4 * sW).toSeq.map(_.mkString("")).mkString("\n"))
    golden
  }

  def compareResults(utArray: ArrayBuffer[Int], refArray: ArrayBuffer[Int]) = {
    require(utArray.size == refArray.size, "two array's size is not match !")
    var ret = true
    utArray.foreach { i =>
      if (utArray(i) != refArray(i)) {
        ret = false
      }
    }
    ret
  }
}

class InterpolationStep3Test extends AnyFunSuite {
  test("Test InterpolationStep3 5 * 5 ") {
    val testCases = ArrayBuffer.fill(10 * 10)(nextInt(32) + 1)
    sim3Funcs.startSim(16, 5, 5, sim2Funcs.getGolden(16, 5, 5, testCases), sim3Funcs.getGolden(16, 5, 5, sim2Funcs.getGolden(16, 5, 5, testCases)), true)

  }
  test("Test InterpolationStep3 Randomly !") {
    val h         = nextInt(541)
    val w         = nextInt(961)
    val thd       = nextInt(201)
    val testCases = ArrayBuffer.fill(4 * h * w)(nextInt(255) + 1)
    sim3Funcs.startSim(thd, h, w, sim2Funcs.getGolden(thd, h, w, testCases), sim3Funcs.getGolden(thd, h, w, sim2Funcs.getGolden(thd, h, w, testCases)), true)
  }
}
