package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import breeze.linalg._
import breeze.numerics.sqrt
import breeze.signal._
import com.github.tototoshi.csv._

import java.io.File
import java.lang.Thread.sleep
import scala.io.StdIn

object ILASync {

  def bits768ToInts(bits: String) = {
    BigInt(bits, 16)
      .toString(2).padToLeft(768, '0')
      .grouped(6).toSeq
      .map(bits => BigInt(bits, 2))
      .map(_.toInt).reverse
  }

  def sync(rxFile: String, ftnParams: FtnParams): (Array[Int], Int) = {

    // vec
    val rxFull = CSVReader.open(new File(rxFile)).all().tail.flatMap(_.slice(3, 131)).map(str => BigInt(str, 16).toInt)
      .map(_ - 32).toArray

    var success = false
    var frame = 0
    val validWithCP = if (ftnParams.end == 256) 532 else 470
    val packedLength = validWithCP * 16 + 532 * 2
    val threshold = if (ftnParams.end == 256) 60 else 40
    var lag = 0

    while (!success) {

      if (frame >= 25) throw new IllegalArgumentException("failed to sync")

      val start = 1064 + frame * packedLength
      //      val preamble = ftnParams.txPacked.flatten.slice(start, start + 128).toArray.map(_.toDouble - 32.0)
      val frameForSync = loadFTN1d[Double]("txPackedAll").map(_.toInt).slice(start, start + 128)

      val rxPower = rootMeanSquare(new DenseVector(rxFull.map(_.toDouble)))
      eng.putVariable("rx", rxFull.map(_ / rxPower))

      val preamblePower = rootMeanSquare(new DenseVector(frameForSync))
      eng.putVariable("tx", frameForSync.map(_ / preamblePower))

      eng.eval(s"[cor, lags] = xcorr(rx, tx);")
      eng.eval(s"plot(lags, cor);")

      val cor = eng.getVariable[Array[Double]]("cor")
      val lags = eng.getVariable[Array[Double]]("lags").map(_.toInt)
      val candidate = cor.zip(lags).maxBy(_._1)._2
      val maxFirst = cor.sorted.reverse.apply(0)
      val maxSecond = cor.sorted.reverse.apply(1)
      val candidateSecond = cor.zip(lags).find(_._1 == maxSecond).get._2
//      if (maxFirst > threshold) {
//        println(maxFirst, maxSecond, candidate, candidateSecond)
//        sleep(5000)
//      }
      if (maxFirst > threshold && ((maxFirst - maxSecond > 5) || (candidate - candidateSecond).abs < 10) && candidate + packedLength <= lags.max && candidate - 1064 >= 0) {
        lag = cor.zip(lags).maxBy(_._1)._2
        eng.eval("cd ~/FTN326/sync")
        eng.eval(s"saveas(gcf, '$rxFile.png')")
        success = true
      }
      else frame += 1


      //    val lag = {
      //      val maxLag = cor.zip(lags).take(10240 * 15).maxBy(_._1)._2
      //      val maxId = maxLag - lags.head
      //      if (cor(maxId + preambleWithCP) > cor(maxId) * 0.5) maxLag
      //      else if (cor(maxId - preambleWithCP) > cor(maxId) * 0.5) maxLag - preambleWithCP
      //      else throw new IllegalArgumentException("failed to sync by current strategy")
      //    }
    }

    val start = lag - 1064
    println(s"range: ${rxFull.length}, start: $start, end: ${start + packedLength}")
    (rxFull.slice(start, start + packedLength), frame)
  }

  def apply(start: Int, end: Int, postFix: String): Unit = {

    // get the params before using syncData
    val ftnParams = FtnParams(start, end, doBitAlloc = false)
    val csvName = s"ila${start}_${end}_$postFix.csv"
    // get syncData
    val syncData = sync(csvName, ftnParams)

    val preambleWithCP = ftnParams.fftSize + ftnParams.cpLength
    val validWithCP = ftnParams.channelInfo.validFFTNum + ftnParams.cpLength

    val syncDataForRx: Seq[Seq[BigInt]] = {
      val (preamble, data) = syncData._1.map(BigInt(_)).toSeq.splitAt(2 * preambleWithCP)
      val preambleWithOutCP = preamble.grouped(preambleWithCP).toSeq.flatMap(_.slice(ftnParams.cpLength / 2, ftnParams.cpLength / 2 + ftnParams.fftSize))
      val dataWithoutCp = data.grouped(validWithCP).toSeq.flatMap(_.slice(ftnParams.cpLength / 2, ftnParams.cpLength / 2 + ftnParams.channelInfo.validFFTNum))
      (preambleWithOutCP ++ dataWithoutCp).grouped(128).toSeq
    }

    eng.eval("cd ~/FTN326")
    eng.putVariable("syncData", syncDataForRx.flatten.map(_.toInt).toArray)
    eng.eval("save syncData syncData")

    // regenerate circuit on board
    doFlowPeekPokeTest(
      dut = RxFull(64, 5)(ftnParams), name = "testSyncRx",
      testCases = syncDataForRx, golden = ftnParams.rxFinalDecodedAll,
      testMetric = TestMetric.SAME
    )

    printlnGreen("rerun matlab with syncData to get bitAlloc")
    val ftnParamsAfterAlloc = FtnParams(start, end, doBitAlloc = true, useSyncData = true)

    eng.eval("cd ~/FTN326/allocs")
    val bitAllocName = s"bitAlloc${start}_${end}_$postFix"
    eng.putVariable(bitAllocName, ftnParamsAfterAlloc.channelInfo.bitAlloc.map(_.toDouble))
    printlnGreen(s"bit Alloc: ${ftnParamsAfterAlloc.channelInfo.bitAlloc.mkString(" ")}")
    eng.eval(s"save $bitAllocName $bitAllocName")
    val powAllocName = s"powAlloc${start}_${end}_$postFix"
    eng.putVariable(powAllocName, ftnParamsAfterAlloc.channelInfo.powAlloc.map(sqrt(_)))
    printlnGreen(s"pow Alloc: ${ftnParamsAfterAlloc.channelInfo.powAlloc.mkString(" ")}")
    printlnGreen(s"pow max: ${ftnParamsAfterAlloc.channelInfo.powAlloc.max}")
    printlnGreen(s"pow min: ${ftnParamsAfterAlloc.channelInfo.powAlloc.min}")
    eng.eval(s"save $powAllocName $powAllocName")

    eng.eval(s"plot($bitAllocName)")
    eng.eval(s"plot(bitAlloc);\nsaveas(gcf, '$bitAllocName.png')")
    eng.eval(s"plot($powAllocName)")
    eng.eval(s"plot(powAlloc);\nsaveas(gcf, '$powAllocName.png')")
  }

  def main(args: Array[String]): Unit = {

    //    sync(s"example.csv", FtnParams(2, 256, doBitAlloc = false))
    //    sync(s"extraSync.csv", FtnParams(3, 226, doBitAlloc = false))
    //    sync(s"extraSyncVec.csv", FtnParams(3, 226, doBitAlloc = false))

    //    (13 to 20).foreach(i => sync(s"ila_3_226_N${i}_20KM_r0.csv", FtnParams(3, 226, doBitAlloc = true)))
    //    ((13 to 14) ++ (16 to 20)).foreach(i => sync(s"ila_2_256_N${i}_20KM_r0.csv", FtnParams(2, 256, doBitAlloc = true)))
    //    (15 to 15).foreach(i => sync(s"ila_2_256_N${i}_20KM_r0.csv", FtnParams(2, 256, doBitAlloc = true)))
    //    (13 to 20).foreach(i => sync(s"ila_3_226_N${i}_0KM_r0.csv", FtnParams(3, 226, doBitAlloc = true)))
    (13 to 20).foreach(i => sync(s"ila_2_256_N${i}_0KM_r0.csv", FtnParams(2, 256, doBitAlloc = true)))


    //    (13 until 19).foreach { i => sync(s"3_226_N${i}_0KM", FtnParams(3,226,doBitAlloc = false)) }
    //    (13 until 19).foreach { i => sync(s"2_256_N${i}_0KM", FtnParams(2,256,doBitAlloc = false)) }
    //    (13 until 19).foreach { i => sync(s"3_226_N${i}_20KM", FtnParams(3,226,doBitAlloc = false)) }
    //    (13 until 19).foreach { i => sync(s"2_256_N${i}_20KM", FtnParams(2,256,doBitAlloc = false)) }

    //    apply(2, 256, "N15")
    //    apply(3, 226, "N15")
    //    apply(2, 256, "N15_20KM")
    //    apply(3, 226, "N15_20KM")
  }
}