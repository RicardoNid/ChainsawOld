package Chainsaw.crypto.ReedSolomon

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.sim._
import spinal.core._
import spinal.lib._

class EncodingTest extends AnyFlatSpec {

  SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new Encoding(15, 11, 4))
    .doSim { dut =>
      import dut._
      println(dut.coff.mkString(" "))
      clockDomain.forkStimulus(10)
      io.input   #= 0
      io.control #= false
      clockDomain.waitSampling()
      Range(1, 12).foreach{i =>
        io.input #= i
        io.control #= true
        clockDomain.waitSampling()
      }
      io.input #= 0
      io.control #= false
      clockDomain.waitSampling(6)
    }
}
