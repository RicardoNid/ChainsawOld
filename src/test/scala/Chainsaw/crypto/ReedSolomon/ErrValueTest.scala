package Chainsaw.crypto.ReedSolomon

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._

class ErrValueTest extends AnyFlatSpec {

  SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new ErrValue())
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(10)
      io.omegaRLambda.valid             #= false
      io.errValue.ready                 #= false
      io.omegaRLambda.payload.foreach(_ #= 0)
      clockDomain.waitSampling()
      io.omegaRLambda.valid #= true
      io.errValue.ready     #= true
      val lambda = Seq(14, 15, 13, 9, 1, 2, 4, 8, 3, 6, 12, 11, 5, 10, 7)
      io.omegaRLambda.payload.zip(Seq(3, 14).reverse).foreach { case (p, d) => p #= d }
      clockDomain.waitSampling()
      lambda.foreach { l =>
        io.omegaRLambda.payload.last #= l
        clockDomain.waitSampling()
      }
    }
}
