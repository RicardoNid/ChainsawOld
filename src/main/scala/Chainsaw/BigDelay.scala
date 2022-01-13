package Chainsaw

import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

/** wrapper for Delay with a big width/depth, delay is described by Mem rather than Reg which would make simulation very slow
 *
 */
// TODO: support init(BigMem need to support init)
object BigDelay {
  def apply[T <: Data](that: Vec[T], cycleCount: Int, when: Bool = null): Vec[T] = {
    val counter = CounterFreeRun(cycleCount)
    val mem = BigMem(HardType(that), cycleCount)

    if (when == null) mem.write(counter.value, that) else mem.write(counter.value, that, when)
    mem.readSync(counter.valueNext)
  }

  // simple test for BigDelay
  def main(args: Array[String]): Unit = {
    doFlowPeekPokeTest(
      name = "testBigDelay", dut = new Component with DSPTestable[Vec[UInt], Vec[UInt]] {
        override val dataIn = slave Stream Vec(UInt(8 bits), 1)
        override val dataOut = master Stream Vec(UInt(8 bits), 1)
        override val latency = 16
        val delay = BigDelay(dataIn.payload, 16)
        dataIn.ready := True
        dataOut.payload := delay
        dataOut.valid := Delay(dataIn.valid, 16, init = False)
      },
      testCases = (0 until 32).map(BigInt(_)).map(Seq(_)),
      golden = (0 until 32).map(BigInt(_)).map(Seq(_)),
      testMetric = TestMetric.SAME
    )
  }
}
