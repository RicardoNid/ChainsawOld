package Chainsaw.memories

import Chainsaw._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import mWnRMode._

class mWnRRAMTest extends AnyFunSuite {

  // FIXME: fix this after 11.15

//  test("testmWnRBehavior"){
//    def mWnRSim(writeNum: Int, readNum: Int, testMode: mWnRMode, caseNum: Int = 100) = {
//      SimConfig.withWave.compile(new mWnRDUT(writeNum, readNum, testMode)).doSimUntilVoid { dut =>
//        import dut._
//        if (testMode == MULTIPUMPING) {
//          ramClockDomain.forkStimulus(2)
//          fork {
//            globalClockDomain.clockSim #= false
//            ramClockDomain.waitSampling()
//            while (true) {
//              globalClockDomain.risingEdge()
//              sleep(writeNum + readNum)
//              globalClockDomain.fallingEdge()
//              sleep(writeNum + readNum)
//            }
//          }
//        } else {
//          globalClockDomain.forkStimulus(2 * (writeNum + readNum))
//        }
//        import dut.globalClokingArea._
//
//        val randomData = Seq.tabulate(caseNum, writeNum)((_, _) => DSPRand.nextInt(1 << 18))
//        val randomAddr = Seq.tabulate(caseNum, writeNum)((i, j) => i + j)
//
//        def writeAll(num: Int) = io.writes.zipWithIndex.foreach { case (port, i) => port.simWrite(randomAddr(num)(i), randomData(num)(i)) }
//        def readAll(num: Int) = io.reads.zipWithIndex.foreach { case (port, i) => port.simRead(randomAddr(num)(i % m) % (1 << 10)) }
//
//        val write = fork {
//          globalClockDomain.waitSampling() // start at rising edge 0
//          (0 until caseNum).foreach { i =>
//            writeAll(i)
//            globalClockDomain.waitSampling()
//          }
//        }
//        val read = fork {
//          globalClockDomain.waitSampling(2) // start at rising edge 1
//          (0 until caseNum).foreach { i =>
//            readAll(i)
//            globalClockDomain.waitSampling()
//          }
//          printlnGreen(s"$caseNum cases on mode $mode succeed")
//          simSuccess()
//        }
//        val printer = fork {
//          var caseCount = 0
//          globalClockDomain.waitSampling(4) // start at rising edge 3
//          if (mode == MULTIPUMPING) caseCount += 1 // as MULTIPUMPING appears to be asynchronously read
//          while (caseCount < caseNum) {
//            //            println(s"check at time ${simTime()}")
//            assert(io.reads.zipWithIndex.forall { case (port, i) => port.dataOut.toBigInt.toInt == randomData(caseCount)(i % m) })
//            globalClockDomain.waitSampling()
//            caseCount += 1
//          }
//        }
//      }
//    }
//    mWnRMode.values.foreach(mode => mWnRSim(1, 3, mode, 1000))
//  }
}
