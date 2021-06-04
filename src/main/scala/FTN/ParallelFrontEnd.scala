package FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class ParallelFrontEnd extends DSPDUTTiming[Bits, Bits] {
  val start = in Bool
  override val input: Bits = in Bits (4 bits) // not used, just for DSPDUT template
  override val output: Bits = out Bits (896 bits)
  val valid = out Bool

  val testData =
    (0 until 10).map(_ =>
      (0 until 7168).map(_ => DSPRand.nextInt(2)).toArray).toArray

  val frameBuffer = new FrameBuffer(testData)
  val parallelConvenc = new ParallelConvenc

  frameBuffer.input := start
  parallelConvenc.start := start

  parallelConvenc.input := frameBuffer.output
  output := parallelConvenc.output

  valid := parallelConvenc.valid

  override val timing: TimingInfo = TimingInfo(166, 160, 7, 200) // 10 frames as a group input interval = 6 + 10 * 160
}

object ParallelFrontEnd {
  def main(args: Array[String]): Unit = {
    GenRTL(new ParallelFrontEnd)

    val container = ArrayBuffer[Array[Boolean]]()

    SimConfig.withWave.compile(new ParallelFrontEnd).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling(5)
      start #= true
      (0 until 160).foreach { _ =>
        println(output.toBigInt.toString(2))
        clockDomain.waitSampling()
      }
    }

  }
}
