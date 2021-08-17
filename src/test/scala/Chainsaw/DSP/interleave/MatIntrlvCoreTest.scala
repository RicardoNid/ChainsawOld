package Chainsaw.DSP.interleave

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import spinal.core

import scala.collection.mutable.ArrayBuffer

class MatIntrlvCoreTest extends AnyFunSuite {

  test("test the core part of parallel interleaver") {
    val dataWidth = 8
    val dataType = HardType(UInt(dataWidth bits))
    def testMatIntrlvCoreHardware(row: Int, col: Int) = {
      SimConfig.withWave.compile(new MatIntrlvCore(row, col, dataType)).doSim { dut =>

        val testData0, testData1 = Seq.tabulate(row, col)((_, _) => DSPRand.nextInt(1 << dataWidth))
        val transposed0 = Algos.matIntrlv2D2D(testData0, row, col)
        val transposed1 = Algos.matIntrlv2D2D(testData1, row, col)

        import dut._
        def doRead() = {
          dataOut.ready #= true
          dataOut.payload.map(_.toInt)
        }

        def doWrite(values: Seq[Int]) = {
          dataIn.valid #= true
          dataIn.payload.zip(values).foreach { case (int, i) => int #= i }
        }

        def haltRead() = dataOut.ready #= false
        def haltWrite() = dataIn.valid #= false

        clockDomain.forkStimulus(2)
        dataIn.valid #= false
        dataOut.ready #= false
        clockDomain.waitSampling()

        val dutResult = ArrayBuffer[Seq[Int]]()
        val monitor = fork {
          while (true) {
            if (dataOut.valid.toBoolean) dutResult += dataOut.payload.map(_.toInt)
            clockDomain.waitSampling()
          }
        }

        // input
        testData0.foreach { data =>
          doWrite(data)
          clockDomain.waitSampling()
        }

        // input + output
        val period = dut.row max dut.col
        (0 until period).foreach { i =>
          if (i < dut.row) doWrite(testData1(i))
          if (i < dut.col) doRead()
          clockDomain.waitSampling()
        }

        // check 0
        clockDomain.waitSampling()
        transposed0.zip(dutResult).foreach { case (ints, ints1) => assertResult(expected = ints)(actual = ints1) }
        dutResult.clear()

        // output
        (0 until dut.col).foreach { _ =>
          doRead()
          clockDomain.waitSampling()
        }

        // check 1
        transposed1.zip(dutResult).foreach { case (ints, ints1) => assertResult(expected = ints)(actual = ints1) }

        haltRead()
        haltWrite()
        clockDomain.waitSampling(20)
      }
    }

    testMatIntrlvCoreHardware(10, 10)
    printlnGreen("test when row = col, passed")
    testMatIntrlvCoreHardware(7, 9)
    printlnGreen("test when row < col, passed")
    testMatIntrlvCoreHardware(9, 7)
    printlnGreen("test when row > col, passed")
  }

  test("test the general parallel interleaver") {
    val dataWidth = 8
    val dataType = HardType(UInt(dataWidth bits))
    def testMatIntrlvHardware(row: Int, col: Int, pFIn: Int, pFOut: Int) = {
      SimConfig.withWave.compile(new MatIntrlv(row, col, pFIn, pFOut, dataType)).doSim { dut =>

        val testData0, testData1 = Seq.tabulate(row, col)((_, _) => DSPRand.nextInt(1 << dataWidth))
        val transposed0 = Algos.matIntrlv2D2D(testData0, row, col)
        val transposed1 = Algos.matIntrlv2D2D(testData1, row, col)

        import dut._
        def doRead() = {
          dataOut.ready #= true
          dataOut.payload.map(_.toInt)
        }

        def doWrite(values: Seq[Int]) = {
          dataIn.valid #= true
          dataIn.payload.zip(values).foreach { case (int, i) => int #= i }
        }

        def haltRead() = dataOut.ready #= false
        def haltWrite() = dataIn.valid #= false

        clockDomain.forkStimulus(2)
        dataIn.valid #= false
        dataOut.ready #= false
        clockDomain.waitSampling()

        val dutResult = ArrayBuffer[Seq[Int]]()
        val monitor = fork {
          while (true) {
            if (dataOut.valid.toBoolean) dutResult += dataOut.payload.map(_.toInt)
            clockDomain.waitSampling()
          }
        }

        // input
        testData0.foreach { data =>
          doWrite(data)
          clockDomain.waitSampling()
        }

        // input + output
        val period = dut.row max dut.col
        (0 until period).foreach { i =>
          if (i < dut.row) doWrite(testData1(i))
          if (i < dut.col) doRead()
          clockDomain.waitSampling()
        }

        // check 0
        clockDomain.waitSampling()
        transposed0.zip(dutResult).foreach { case (ints, ints1) => assertResult(expected = ints)(actual = ints1) }
        dutResult.clear()

        // output
        (0 until dut.col).foreach { _ =>
          doRead()
          clockDomain.waitSampling()
        }

        // check 1
        transposed1.zip(dutResult).foreach { case (ints, ints1) => assertResult(expected = ints)(actual = ints1) }

        haltRead()
        haltWrite()
        clockDomain.waitSampling(20)
      }
    }

    testMatIntrlvHardware(8, 8, 16, 16)
  }
}
