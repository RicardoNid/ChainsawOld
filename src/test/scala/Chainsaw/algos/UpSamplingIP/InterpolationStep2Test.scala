package Chainsaw.algos.UpSamplingIP

import spinal.lib._
import spinal.core._
import spinal.core.sim._
import org.slf4j._
import org.scalatest.funsuite._
import scala.collection.mutable._
import scala.util.Random._

object sim2Funcs {
  def startSim(threshold: Int, sH: Int, sW: Int, testCases: ArrayBuffer[Int], golden: ArrayBuffer[Int], isPrint: Boolean = false) = {
    val compiled   = SimConfig.withFstWave.compile(InterpolationStep2(IPConfig(sH, sW)))
    val results    = ArrayBuffer[Int]()
    val rowEndOuts = ArrayBuffer[Boolean]()
    val validOuts  = ArrayBuffer[Boolean]()
    var inCount    = -1
    compiled.doSimUntilVoid { dut =>
      val logger = LoggerFactory.getLogger(s"Test : InterpolationStep2")

      dut.io.dataOut.ready #= false
      dut.io.rowEndIn      #= false
      dut.io.frameStartIn  #= false
      dut.io.StartIn       #= false
      dut.io.widthIn       #= 1
      dut.io.heightIn      #= 1

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
          if (inCount == 0) {
            dut.io.frameStartIn #= true
          } else {
            dut.io.frameStartIn #= false
          }
          if ((inCount + 1) % (2 * sW) == 0) {
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
            dut.io.inpThreeCompleteIn #= true
            dut.io.StartIn            #= false
            dut.clockDomain.waitSampling(2)
            if (isPrint) {
              val formatTestCases  = testCases.map(_.toString.padTo(5, ' ')).grouped(2 * sW).toSeq.map(_.mkString("")).mkString("\n")
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
          } else {
            dut.io.inpThreeCompleteIn #= false
          }
          //sleep(2)
          dut.clockDomain.waitSampling()
        }
      }

    }
  }

  def getGolden(threshold: Int, sH: Int, sW: Int, testCases: ArrayBuffer[Int], isPrint: Boolean = false) = {
    val golden          = ArrayBuffer[Int]()
    val rows            = testCases.grouped(2 * sW).toBuffer
    val inpFirstPixels  = ArrayBuffer.fill(sH)(ArrayBuffer[Int]())
    val inpSecondPixels = ArrayBuffer.fill(sH)(ArrayBuffer[Int]())
    val inpThirdPixels  = ArrayBuffer.fill(sH)(ArrayBuffer[Int]())
    val inpFourthPixels = ArrayBuffer.fill(sH)(ArrayBuffer[Int]())

    Range(0, sH).foreach{ fIdx =>
      val lastRow = 2 * fIdx
      Range(0, 4 * sW).foreach{ i =>
        if(i % 4 == 0 || i % 4 == 2){
          inpFirstPixels(fIdx) += rows(lastRow)(i / 2)
        }else{
          if(i % 4 == 1){
            inpFirstPixels(fIdx) += rows(lastRow)((i - 1) / 2)
          }else{
            if(i + 1 <= 4 * sW - 1) {
              if ((rows(lastRow)((i - 3) / 2) - rows(lastRow)((i + 1) / 2)).abs >= threshold) {
                inpFirstPixels(fIdx) += 0
              }else{
                inpFirstPixels(fIdx) += (rows(lastRow)((i - 3) / 2) + rows(lastRow)((i + 1) / 2)) / 2
              }
            }else{
              inpFirstPixels(fIdx) += rows(lastRow)((i - 3) / 2)
            }
          }
        }
      }
    }

    Range(0, sH).foreach { sIdx =>
      val lastRow = 2 * sIdx
      var nextRow = 2 * sIdx + 2
      if (nextRow > 2 * sH - 1) { nextRow = nextRow - 2 }
      Range(0, 4 * sW).foreach { i =>
        if (i % 4 == 0 || i % 4 == 1 || i % 4 == 2) {
          inpSecondPixels(sIdx) += rows(lastRow)((i - i % 4) / 2)
          if ((rows(lastRow)((i - i % 4) / 2) - rows(nextRow)((i - i % 4) / 2)).abs >= threshold) {
            inpFourthPixels(sIdx) += 0
          } else {
            inpFourthPixels(sIdx) += (rows(lastRow)((i - i % 4) / 2) + rows(nextRow)((i - i % 4) / 2)) / 2
          }
        } else {
          if (i + 1 <= 4 * sW - 1) {
            val mDiff = (rows(lastRow)((i - 3) / 2) - rows(nextRow)((i + 1) / 2)).abs
            val cDiff = (rows(nextRow)((i - 3) / 2) - rows(lastRow)((i + 1) / 2)).abs
            if((rows(lastRow)((i - 3) / 2) - rows(lastRow)((i + 1) / 2)).abs >= threshold){
              inpSecondPixels(sIdx) += 0
            }else{
              inpSecondPixels(sIdx) += (rows(lastRow)((i - 3) / 2) + rows(lastRow)((i + 1) / 2)) / 2
            }

            if (Seq(mDiff, cDiff).min >= threshold) {
              inpFourthPixels(sIdx) += 0
            } else {
              if (mDiff >= cDiff) {
                inpFourthPixels(sIdx) += (rows(nextRow)((i - 3) / 2) + rows(lastRow)((i + 1) / 2)) / 2
              } else {
                inpFourthPixels(sIdx) += (rows(lastRow)((i - 3) / 2) + rows(nextRow)((i + 1) / 2)) / 2
              }
            }
          } else {
            inpSecondPixels(sIdx) += rows(lastRow)((i - 3) / 2)
            if ((rows(lastRow)((i - 3) / 2) - rows(nextRow)((i - 3) / 2)).abs >= threshold) {
              inpFourthPixels(sIdx) += 0
            } else {
              inpFourthPixels(sIdx) += (rows(lastRow)((i - 3) / 2) + rows(nextRow)((i - 3) / 2)) / 2
            }
          }
        }
      }
    }

    Range(0, sH).foreach { tIdx =>
      val lastRow = 2 * tIdx + 1
      Range(0, 4 * sW).foreach { i =>
        if(i % 4 == 0 || i % 4 == 2){
          inpThirdPixels(tIdx) += rows(lastRow)(i / 2)
        }else{
          if(i % 4 == 1){
            inpThirdPixels(tIdx) += rows(lastRow - 1)((i - 1) / 2)
          }else{
            if(i + 1 <= 4 * sW - 1) {
              if ((rows(lastRow - 1)((i - 3) / 2) - rows(lastRow - 1)((i + 1) / 2)).abs >= threshold) {
                 inpThirdPixels(tIdx) += 0
              }else{
                inpThirdPixels(tIdx) += (rows(lastRow - 1)((i - 3) / 2) + rows(lastRow - 1)((i + 1) / 2)) / 2
              }
            }else{
              inpThirdPixels(tIdx) += rows(lastRow - 1)((i - 3) / 2)
            }
          }
        }
      }
    }

//    println(rows.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))
//    println("----")
//    println(inpFirstPixels.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))
//    println("----")
//    println(inpSecondPixels.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))
//    println("----")
//    println(inpThirdPixels.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))
//    println("----")
//    println(inpFourthPixels.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))

    Range(0, 4 * sH).foreach { h =>
      h % 4 match {
        case 0 => golden ++= inpFirstPixels(h / 4)
        case 1 => golden ++= inpSecondPixels(h / 4)
        case 2 => golden ++= inpThirdPixels(h / 4)
        case 3 => golden ++= inpFourthPixels(h / 4)
      }
    }

//    println("***")
//    println(golden.map(_.toString.padTo(3, ' ')).grouped(4 * sW).toSeq.map(_.mkString("")).mkString("\n"))

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

class InterpolationStep2Test extends AnyFunSuite {
  test("Test InterpolationStep2 5 * 5 ") {
    val testCases = ArrayBuffer.fill(10 * 10)(nextInt(32) + 1)
    sim2Funcs.startSim(16, 5, 5, testCases, sim2Funcs.getGolden(16, 5, 5, testCases), true)

  }
  test("Test InterpolationStep2 Randomly !") {
    val h         = nextInt(541)
    val w         = nextInt(961)
    val thd       = nextInt(201)
    val testCases = ArrayBuffer.fill(4 * h * w)(nextInt(255) + 1)
    sim2Funcs.startSim(thd, h, w, testCases, sim2Funcs.getGolden(thd, h, w, testCases), true)
  }
}
