package Chainsaw.algos.UpSamplingIP

import org.scalatest.funsuite._
import org.slf4j._
import spinal.core.sim._

import scala.collection.mutable._
import scala.util.Random._

object sim1Funcs {
  def startSim(threshold: Int, sH: Int, sW: Int, testCases: ArrayBuffer[Int], golden: ArrayBuffer[Int], isPrint: Boolean = false) = {
    val compiled   = SimConfig.withFstWave.compile(InterpolationStep1(IPConfig(sH, sW)))
    val results    = ArrayBuffer[Int]()
    val rowEndOuts = ArrayBuffer[Boolean]()
    var inCount    = -1
    compiled.doSimUntilVoid { dut =>
      val logger = LoggerFactory.getLogger(s"Test : InterpolationSep1")

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
        dut.io.thresholdIn     #= threshold
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
          if ((inCount + 1) % sW == 0) {
            dut.io.rowEndIn #= true
          } else {
            dut.io.rowEndIn #= false
          }
          dut.clockDomain.waitSamplingWhere(dut.io.dataIn.valid.toBoolean && dut.io.dataIn.ready.toBoolean)
          inCount += 1
          if (inCount == sW * sH) { dut.io.rowEndIn #= false }
        }

      }


      val getSimResult = fork {
        while (true) {
          if (results.length == 4 * sH * sW) {
            dut.io.inpTwoCompleteIn #= true
            dut.io.inpThreeCompleteIn #= true
            dut.io.StartIn    #= false
            dut.clockDomain.waitSampling(2)
            if (isPrint) {
              val formatTestCases  = testCases.map(_.toString.padTo(5, ' ')).grouped(sW).toSeq.map(_.mkString("")).mkString("\n")
              val formatGolden     = golden.map(_.toString.padTo(5, ' ')).grouped(2 * sW).toSeq.map(_.mkString("")).mkString("\n")
              val formatResults    = results.map(_.toString.padTo(5, ' ')).grouped(2 * sW).toSeq.map(_.mkString("")).mkString("\n")
              val formatRowEndOuts = rowEndOuts.map(_.toString.padTo(7, ' ')).grouped(2 * sW).toSeq.map(_.mkString("")).mkString("\n")
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
            dut.io.inpTwoCompleteIn #= false
          }
          //sleep(2)
          dut.clockDomain.waitSampling()
        }
      }

    }
  }

  def getGolden(threshold: Int, sH: Int, sW: Int, testCases: ArrayBuffer[Int], isPrint: Boolean = false) = {
    val golden        = ArrayBuffer[Int]()
    val rows          = testCases.grouped(sW).toBuffer
    val interRows     = rows.zip(rows.tail ++ rows.slice(sH - 1, sH))
    val inpEvenPixels = ArrayBuffer.fill(sH)(ArrayBuffer[Int]())
    val inpOddPixels  = ArrayBuffer.fill(sH)(ArrayBuffer[Int]())
    var eIdx          = 0
    var oIdx          = 0

    rows.foreach { r =>
      val lefts  = r
      val rights = r.tail ++ r.slice(sW - 1, sW)
      lefts.zip(rights).map { case (l, r) =>
        if ((l - r).abs >= threshold) {
          inpEvenPixels(eIdx) += l
        } else {
          inpEvenPixels(eIdx) += (l + r) / 2
        }
      }
      eIdx += 1
    }

    interRows.foreach { case (ints, ints1) =>
      Range(0, 2 * sW).foreach { i =>
        if (i % 2 == 0) {
          if ((ints(i / 2) - ints1(i / 2)).abs >= threshold) {
            inpOddPixels(oIdx) += ints(i / 2)
          } else {
            inpOddPixels(oIdx) += (ints(i / 2) + ints1(i / 2)) / 2
          }
        } else {
          if (i + 1 <= 2 * sW - 1) {
            val mainDiff  = (ints((i - 1) / 2) - ints1((i + 1) / 2)).abs
            val countDiff = (ints1((i - 1) / 2) - ints((i + 1) / 2)).abs
            val minDiff   = Seq(mainDiff, countDiff).min
            if (minDiff >= threshold) {
              if (mainDiff >= countDiff) {
                inpOddPixels(oIdx) += ints((i - 1) / 2)
              } else {
                inpOddPixels(oIdx) += ints1((i - 1) / 2)
              }
            } else {
              if (mainDiff >= countDiff) {
                inpOddPixels(oIdx) += (ints1((i - 1) / 2) + ints((i + 1) / 2)) / 2
              } else {
                inpOddPixels(oIdx) += (ints((i - 1) / 2) + ints1((i + 1) / 2)) / 2
              }
            }
          } else {
            val diff = (ints((i - 1) / 2) - ints1((i - 1) / 2)).abs
            if (diff >= threshold) {
              inpOddPixels(oIdx) += ints((i - 1) / 2)
            } else {
              inpOddPixels(oIdx) += (ints((i - 1) / 2) + ints1((i - 1) / 2)) / 2
            }
          }
        }
      }
      oIdx += 1
    }

//    println(inpEvenPixels.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))
//    println(inpOddPixels.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))

    val realRows = ArrayBuffer.fill(sH)(ArrayBuffer[Int]())
    Range(0, sH).foreach { i =>
      Range(0, 2 * sW).foreach { j =>
        if (j % 2 == 0) {
          realRows(i) += rows(i)(j / 2)
        } else {
          realRows(i) += inpEvenPixels(i)((j - 1) / 2)
        }
      }
    }
    Range(0, 2 * sH).foreach { h =>
      if (h % 2 == 0) {
        golden ++= realRows(h / 2)
      } else {
        golden ++= inpOddPixels((h - 1) / 2)
      }
    }
//    println(realRows.map(_.map(_.toString.padTo(3, ' ')).mkString("")).mkString("\n"))
//    println(golden.map(_.toString.padTo(3, ' ')).grouped(2 * sW).toSeq.map(_.mkString("")).mkString("\n"))
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

class InterpolationStep1Test extends AnyFunSuite {
  test("Test InterpolationStep1 5 * 5 ") {
      val testCases = ArrayBuffer.fill(5 * 5)(nextInt(32))
      sim1Funcs.startSim(16, 5, 5, testCases, sim1Funcs.getGolden(16, 5, 5, testCases), true)
  }
  test("Test InterpolationStep1 Randomly !") {
    val h         = nextInt(541)
    val w         = nextInt(961)
    val thd       = nextInt(201)
    val testCases = ArrayBuffer.fill(h * w)(nextInt(255))
    sim1Funcs.startSim(thd, h, w, testCases, sim1Funcs.getGolden(thd, h, w, testCases), true)
  }
}
