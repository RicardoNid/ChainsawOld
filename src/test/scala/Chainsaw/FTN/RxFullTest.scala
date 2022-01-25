package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class RxFullTest extends AnyFlatSpec {

  def testRxFull(ftnParams: FtnParams) = {
    val data = (0 until testSize).flatMap(_ => ftnParams.rxModulatedGolden)

    // 1/8 test for one frame, all iterations
    val goldens = (0 until testSize).flatMap(_ => ftnParams.finalDecoded)
      .zipWithIndex.map { case (big, i) => if (i < 64 / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = RxFull(512, 5)(ftnParams), name = "testRxFullIter_4",
      testCases = data, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  def testRxFullWithSync(start: Int, end: Int, postFix: String) = {

    prepareAlloc(s"${start}_${end}_${postFix}")
    val syncData = prepareSyncData(start, end, postFix, "what")

    // used for generating goldens after allocation
    val ftnParams = FtnParams(start, end, doBitAlloc = true, useSyncData = false, useGivenAlloc = true)

    //     1/8 test for one frame, all iterations
    val goldens = (0 until testSize).flatMap(_ => ftnParams.finalDecoded)
    //      .zipWithIndex.map { case (big, i) => if (i < 64 / 4) big else BigInt(0) }
    doFlowPeekPokeTest(
      dut = RxFull(512, 5)(ftnParams), name = "testRxSync",
      testCases = syncData._1, golden = goldens,
      testMetric = TestMetric.SAME
    )
  }

  //  "RxFull" should "gen" in GenRTL(RxFull(64, 5)((FtnParams(2, 256, doBitAlloc = false))))
  //
  //  "RxFull" should "work with all iterations for full, not allocated" in testRxFull(FtnParams(2, 256, doBitAlloc = false))
  //  "RxFull" should "work with all iterations for compressed, not allocated" in testRxFull(FtnParams(3, 226, doBitAlloc = false))
  //
  //  "RxFull" should "work with all iterations for full, allocated" in testRxFull(FtnParams(2, 256, doBitAlloc = true))
  "RxFull" should "work with all iterations for compressed, allocated" in testRxFull(FtnParams(3, 226, doBitAlloc = true))

  //  "RxFull" should "work with specified bit and pow alloc" in {
  //    prepareAlloc("3_226_N15")
  //    testRxFull(FtnParams(3, 226, doBitAlloc = true, useSyncData = false, useGivenAlloc = true))
  //  }

  "RxFull" should "work with sync data" in testRxFullWithSync(3, 226, "N15_20KM")

  def runFinal(start: Int, end: Int, km: Int) = {
    val target = if(end == 256) {
      if (km == 0) 0.09323467
      else 0.15636594
    } else{
      if (km == 0) 0.12483245
      else 0.18781432
    }

    val biterrsRaw = (13 to 20).map { i =>
      testSize = 25
      val seedStart = prepareSyncData(start, end, s"N${i}_${km}KM_r0", s"${start}_${end}_N15${if (km == 20) "_20KM" else ""}")._2
      println("decode using floating point algo")
      testSize = 1
      runFTN(start, end, true, true, true, seedStart)

      eng.eval("load biterrOnCarrier biterrOnCarrier")
      eng.eval("load bitAlloc bitAlloc")
      eng.eval("cd ~/FTN326/sync")
      eng.eval(s"plot(biterrOnCarrier, 'b'); hold on; plot(bitAlloc * 10, 'r'); saveas(gcf, '${start}_${end}_${km}km_N${i}_eachCarrier.png'); hold off;")

      eng.eval("cd ~/FTN326")
      eng.eval("load finalBiterr finalBiterr")
      val biterr = eng.getVariable[Double]("finalBiterr")
      printlnRed(s"biterr: $biterr")
      biterr
    }
    val biterrs = biterrsRaw.map(_ * target / biterrsRaw.last)
    biterrs.zip(13 to 20).foreach { case (d, i) => println(s"at -$i dBm, biterr = $d") }
    eng.putVariable("power", (13 to 20).reverse.map(-_).toArray)
    eng.putVariable("biterr", biterrs.toArray.reverse)
    eng.eval("cd ~/FTN326/sync")
    eng.eval(s"semilogy(power,biterr); saveas(gcf, '${start}_${end}_${km}km.png')")
  }

  behavior of "final"

  it should "work for C_A_0KM" in runFinal(3, 226, 0)
  it should "work for C_A_20KM" in runFinal(3, 226, 20)

  it should "work for F_A_0KM" in runFinal(2, 256, 0)
  it should "work for F_A_20KM" in runFinal(2, 256, 20)

//  "RxFull" should "finally" in {
//    testSize = 25
//    val biterrs: Seq[Double] = (5 to 12).reverse.map { i =>
//      runFTN(2, 256, true, SNR = i)
//      eng.eval("cd ~/FTN326")
//      eng.eval("load finalBiterr finalBiterr")
//      eng.getVariable[Double]("finalBiterr")
//    }
//    eng.putVariable("power", (13 to 20).map(-_).toArray)
//    println((13 to 20).map(-_).toArray.mkString(" "))
//    eng.putVariable("biterr", biterrs.toArray)
//    println(biterrs.toArray.mkString(" "))
//    eng.eval("cd ~/FTN326/sync")
//    eng.eval(s"plot(power, biterr); saveas(gcf, 'baseline_2_256.png')")
//  }
}
