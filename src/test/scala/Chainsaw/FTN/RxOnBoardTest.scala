package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import scala.collection.mutable.ArrayBuffer

class RxOnBoardTest extends AnyFlatSpec {

  // prepare data for test

  val txSymbols: Seq[Seq[BComplex]] = loadFTN1d[MComplex]("txMapped").map(_.toBComplex).grouped(256).toSeq.map(_.toSeq)

  val testSize = 1
  val parallelismForTest = 64

  implicit var params: FtnParams = FtnParams(3, 226, true)

  "RxFull" should "work correctly" in {

    SimConfig.withFstWave.compile(RxOnBoard(parallelismForTest)(params)).doSim { dut =>

      // check biterr


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
      import dut.clockDomain
      clockDomain.forkStimulus(2)
      dut.dataOut.ready #= false
      clockDomain.waitSampling()

      // set Monitors
      val afterVitdec = ArrayBuffer[BigInt]()
      dut.dataOut.setMonitor(afterVitdec)

      // poke data
      dut.clockDomain.waitSampling(10000)

      val resultFirst = afterVitdec.take(16)
      reportBiterr(resultFirst)
    }
  }


}
