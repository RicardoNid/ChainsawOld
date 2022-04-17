package Chainsaw.DSP.DAS

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._

class PrePulseSampleTest extends AnyFlatSpec {
  //  new QuartusFlow(PrePulseSample()).impl()
  // 244.68 MHz
  // 24 ALMs
  SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new PrePulseSample())
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(10)
      io.dataIn.payload.raw #= 0
      io.dataIn.valid       #= false
      clockDomain.waitSampling()
      Range(0, 8000).foreach { i =>
        io.dataIn.randomize()
        clockDomain.waitSampling()
      }
    }

}
