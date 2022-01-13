package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class FDEBufferTest extends AnyFlatSpec {

  val data = (0 until 256).map(BigInt(_))
  //  val dataType = HardType(Vec(rxUnitComplexType, 256))
  val dataType = HardType(Vec(UInt(8 bits), 1))

  doFlowPeekPokeTest(
    name = "testLoopBuffer", dut = new Component with DSPTestable[Vec[UInt], Vec[UInt]] {
      val buffer = FDEBuffer(dataType, 16, 4, 5)
      override val dataIn = slave(cloneOf(buffer.dataIn))
      override val dataOut = master(cloneOf(buffer.dataOut))
      override val latency = buffer.latency
      buffer.start := True
      dataIn >> buffer.dataIn
      buffer.dataOut >> dataOut
    },
    testCases = data.map(value => Seq(value)),
    golden = data.grouped(16).toSeq.flatMap(frame => Seq.fill(5 - 1)(frame)).flatten.map(value => Seq(value)),
    testMetric = TestMetric.SAME
  )

}
