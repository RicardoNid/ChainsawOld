package FTN

import Chainsaw._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TrainCtrlTest extends AnyFunSuite {

  test("test TrainCtrl for FTN") {

    val frameSize = 16
    val iterationLatency = 128
    val iteration = 10

    SimConfig.withWave.compile(new TrainCtrl(iterationLatency, frameSize, iteration)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)

      // test of full mode

      val dataInFIFO = mutable.Queue[Int]()

      input.valid #= false
      input.payload #= 0
      output.ready #= true
      clockDomain.waitSampling()

      val dutRet = ArrayBuffer[Int]()
      val monitor = fork {
        if (output.valid.toBoolean) dutRet += output.payload.toInt
      }

      def testFullMode() = {
        val frameCount = 50
        val data = Seq.tabulate(frameCount, frameSize)(_ * 100 + _).flatten
        data.foreach(dataInFIFO.enqueue(_))

        while (dataInFIFO.nonEmpty) {
          if (fire.toBoolean) dataInFIFO.dequeue() // it the data is consumed in the past cycle
          if (dataInFIFO.nonEmpty) input.payload #= dataInFIFO.head
          input.valid #= true
          clockDomain.waitSampling()
        }

        input.valid #= false
        input.payload #= 0
        clockDomain.waitSampling(2000)

        assert(data.zip(dutRet).forall { case (i, i1) => i == i1 }) // make sure that there's no data loss
        printlnGreen("test of full mode, passed")
        dutRet.clear()
      }

      def testRandomMode() = {
        val frameCountForRandom = 10
        val dataForRandom = Seq.tabulate(frameCountForRandom, frameSize)(_ * 100 + _).flatten
        dataForRandom.foreach(dataInFIFO.enqueue(_))

        while (dataInFIFO.nonEmpty) {
          if (dataInFIFO.head % 100 == 0) {
            input.valid #= false
            input.payload #= 0 // empty now
            clockDomain.waitSampling(DSPRand.nextInt(20)) // 0~19 cycles
          }
          if (fire.toBoolean) dataInFIFO.dequeue()
          if (dataInFIFO.nonEmpty) input.payload #= dataInFIFO.head
          input.valid #= true
          clockDomain.waitSampling()
        }

        input.valid #= false
        input.payload #= 0
        clockDomain.waitSampling(2000)

        assert(dataForRandom.zip(dutRet).forall { case (i, i1) => i == i1 }) // make sure that there's no data loss
        printlnGreen("test of random mode, passed")
        dutRet.clear()
      }

      testRandomMode()
    }
  }
}
