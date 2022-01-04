package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec

class LoopBufferTest extends AnyFlatSpec {

  val data = (0 until 256).map(BigInt(_))
  //  val dataType = HardType(Vec(rxUnitComplexType, 256))
  val dataType = HardType(Vec(UInt(8 bits), 1))

  doFlowPeekPokeTest(
    name = "testLoopBuffer", dut = LoopBuffer(dataType, 16, 4, 5),
    testCases = data.map(value => Seq(value)),
    golden = data.grouped(16).toSeq.flatMap(frame => Seq.fill(5)(frame)).flatten.map(value => Seq(value)),
    testMetric = TestMetric.SAME
  )

}
