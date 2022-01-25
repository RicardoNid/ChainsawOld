package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.sys.process._

/** used to test the data order of AUV901 system, it has a same interface as TxOnBoard
 *
 */
case class TestROM() extends Component {

  val leading0s: Seq[Int] = Seq.fill(128)(0)
  ChainsawRand.setSeed(42)
  val sequence: Seq[Int] = (0 until 127 * 128).map(_ => (((ChainsawRand.nextInt(64) - 31.5) * 0.3) + 31.5).toInt) // the sequence which is supposed tobe viewed by upper program
  logger.info(s"part of the test sequence: ${sequence.take(16).mkString(" ")}")
  logger.info(s"test ROM size = ")

  val data: Seq[Vec[UInt]] = (leading0s ++ sequence).grouped(128).toSeq.map(seq => Vec(seq.map(U(_, 6 bits))))
  val rom = Mem(data) // todo: using URAM
  val counter = CounterFreeRun(128)

  val dataRead = rom.readSync(counter.value)
  dataRead.simPublic()

  val dataOut = out Bits (768 bits)
  dataOut := dataRead.asBits
}

object TestROM extends App {

  SimConfig.withFstWave.compile(TestROM()).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling(256)
  }

  GenRTL(TestROM(), name = "TestROM")
}
