package Chainsaw.DSP.DAS

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._

class RowDiffAndUnwrapTest extends AnyFlatSpec {
//  new QuartusFlow(RowDiffAndUnwrap()).impl() // FIXME

  SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new RowDiffAndUnwrap())
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(10)
      io.dataIn.valid       #= false
      io.dataIn.payload.raw #= 0
      clockDomain.waitSampling()
      Range(0, 1000000).foreach { i =>
        io.dataIn.randomize()
        clockDomain.waitSampling()
      }
    }
}
