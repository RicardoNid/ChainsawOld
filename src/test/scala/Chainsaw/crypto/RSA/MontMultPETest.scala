package Chainsaw.crypto.RSA

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core
import spinal.core.sim._
import spinal.sim._
import spinal.core._
class MontMultPETest extends AnyFlatSpec {

  SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new MontMultPE(8))
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(10)
      io.flowIn.data.SWord    #= 0
      io.flowIn.data.YWord    #= 0
      io.flowIn.data.MWord    #= 0
      io.flowIn.control.setXi #= false
      io.xi                   #= 0
      clockDomain.waitSampling()
      (0 until 1000).foreach { i =>
        io.flowIn.randomize()
        io.xi.randomize()
        clockDomain.waitSampling()
      }
    }
}
