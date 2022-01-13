package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._

class Trigger(val trigger: Bool, val count: Int) extends ImplicitArea[Bool] {
  val start = False

  val counter = Counter(count)
  val triggered = counter =/= counter.getZero || trigger
  when(triggered)(counter.increment())

  override def implicitValue: Bool = triggered
}

object Trigger {

  def apply(trigger: Bool, count: Int): Trigger = new Trigger(trigger, count)

  // a simple test for trigger
  def main(args: Array[String]): Unit = {
    SimConfig.withFstWave.compile(
      new Component {
        val dataIn = in Bool()
        val trigger = Trigger(dataIn, 128)
        val dataOut = out Bool()
        dataOut := trigger
      }
    ).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.dataIn #= false
      dut.clockDomain.waitSampling()
      dut.dataIn #= true
      dut.clockDomain.waitSampling()
      dut.dataIn #= false
      dut.clockDomain.waitSampling(64)
      dut.dataIn #= true // until "triggered" state end, the stimulus won't affect the trigger
      dut.clockDomain.waitSampling()
      dut.dataIn #= false
      dut.clockDomain.waitSampling(70) // "triggered" state end
      dut.dataIn #= true
      dut.clockDomain.waitSampling()
      dut.dataIn #= false
      dut.clockDomain.waitSampling(130)
    }

  }
}