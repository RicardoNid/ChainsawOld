package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer

class RxFullTest extends AnyFlatSpec {

  "RxFull" should "work correctly" in {

    // prepare data
    val txBits: Array[Int] = loadFTN1d[Double]("txRaw").map(_.toInt)
    val txSymbols: Seq[Seq[BComplex]] = loadFTN1d[MComplex]("txMapped").map(_.toBComplex).grouped(256).toSeq.map(_.toSeq)

    val rxModulateGolden: Seq[Seq[BigInt]] = {
      val (preamble, data) = loadFTN1d[Double]("rxScaled").splitAt(1024)
      val padded = preamble.grouped(512).map(_.toSeq).toSeq ++ data.grouped(450).map(_.toSeq.padTo(512, 0.0)).toSeq
      padded.flatten.grouped(128).toSeq.map(_.map(value => BigInt(value.toInt)))
    }

    SimConfig.withWave.compile(RxFull(512)).doSim { dut =>

      // check biterr
      def reportBiterr(dutResult: Seq[BigInt]): Unit = {
        val yourRx: Seq[Int] = dutResult.take(16).flatMap(_.toString(2).padToLeft(512, '0').map(_.asDigit))
        val biterr = yourRx.slice(32 * 2, 32 * 226).zip(txBits.slice(32 * 2, 32 * 226)).count { case (rx, tx) => rx != tx } / 8192.0
        logger.info(s"bit err is $biterr")
      }

      // check symbol diff
      def reportSymbolDiff(dutResult: Seq[Seq[BComplex]]): Unit = { // which means they're symbols
        val yourSymbols: Seq[Seq[BComplex]] = dutResult.take(16)
        val symbolsDiff: Double = yourSymbols.zip(txSymbols).map { case (yours, tx) =>
          yours.zip(tx).zipWithIndex
            .filter { case (_, i) => i > 1 && i < 226 }
            .map { case ((y, t), _) => abs(y - t) }.sum
        }.sum
        logger.info(s"symbol diff is $symbolsDiff")
      }

      // init
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dut.dataIn.valid #= false
      dut.dataOut.ready #= false
      clockDomain.waitSampling()

      // set Monitors
      val equalized, afterQammod, afterFft, afterDiff = ArrayBuffer[BComplex]()
      val afterVitdec = ArrayBuffer[BigInt]()

      dut.fft.dataOut.setMonitor(afterFft)

      // poke data
      rxModulateGolden.foreach { data =>

      }

    }
  }

}
