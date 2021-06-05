package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import spinal.lib.StreamFifo

import Chainsaw._
import Chainsaw.Real

/** Depth = 4
 * Currently, without the SERDES, we use implement it as a ROM which contians bits for 10 frames
 * FrameData, data of N frames, a frame is an array of 16 * 448 Int(0 or 1)
 */
class FrameBuffer(FrameData: Array[Array[Int]]) extends DSPDUTTiming[Bool, Vec[Bits]] {

  println(s"first 16 bits: ${FrameData(0).take(16).mkString(" ")}")
  // 1 0 1 0 0 1 0 1 1 0 1 0 0 0 0 1
  // 0 0 1 0 0 1 1 1 1 1 1 1 0 0 0 1 0 0 1 1 0 0 1 0 0 0 0 0 1 1 1 0

  override val input: Bool = in Bool // start signal
  val data = // 10 * 16 * 448 bits, caution, the first frame cycle should contains 0, 16, 32..., rather than 0, 1, 2...
    FrameData.map{frame =>
      val groups = frame.grouped(16).toArray // 448 * 16
      val transposed = (0 until 16).map(i => groups.map(group => group(i))).toArray // 16 * 448
      transposed.map(bitsOneCycle => B(bitsOneCycle.mkString("")))}
  val ROMS = data.map(frame =>
    Mem(Bits(448 bits), initialContent = frame))
  override val output: Vec[Bits] = out Vec(Bits(448 bits), 2)

  output(0) := B(0)
  output(1) := B(0)

  //  val stateCounter = Counter(10)
  val addressCounter = Counter(16)
  val isEven = RegInit(True)

  val fsm = new StateMachine {

    val IDLE = new State() with EntryPoint
    IDLE.whenIsActive(when(input)(goto(INIT)))

    val INIT = new StateDelay(6)
    INIT.whenCompleted(goto(WORKs.head))
    INIT.onExit(addressCounter.clear())
    INIT.whenIsActive(addressCounter.increment())

    val WORKs = (0 until 10).map(_ => new StateDelay(16))
    WORKs.zip(WORKs.tail :+ WORKs.head).foreach { case (pre, next) => pre.whenCompleted(goto(next)) }
    WORKs.foreach(state => state.onExit(isEven := !isEven))
    WORKs.foreach(state => state.whenIsActive(addressCounter.increment()))

    when(isActive(INIT)) {
      output(0) := ROMS(0)(addressCounter.value + U(10))
      output(1) := ROMS(1)(U(0, 4 bits))
    }

    WORKs.init.zipWithIndex.foreach { case (state, i) =>
      state.whenIsActive {
        when(isEven) {
          output(0) := ROMS(i % 10)(addressCounter.value)
          output(1) := ROMS((i + 1) % 10)(addressCounter.value)
        }.otherwise {
          output(0) := ROMS((i + 1) % 10)(addressCounter)
          output(1) := ROMS(i % 10)(addressCounter)
        }
      }
    }
  }

  override val timing: TimingInfo = TimingInfo(1, 1, 1, 1)
}

object FrameBuffer {
  def main(args: Array[String]): Unit = {

    val testData =
      (0 until 10).map(_ =>
        (0 until 7168).map(_ => DSPRand.nextInt(2)).toArray).toArray

    GenRTL(new FrameBuffer(testData))
    SimConfig.withWave.compile(new FrameBuffer(testData)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling(5)
      input #= true
      sleep(1000)
    }
    VivadoSynth(new FrameBuffer(testData))

  }
}
