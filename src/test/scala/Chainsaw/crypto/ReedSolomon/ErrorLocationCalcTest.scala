package Chainsaw.crypto.ReedSolomon

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import scala.util.Random._

class ErrorLocationCalcTest extends AnyFlatSpec {

  SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new ErrorLocationCalc(4))
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(10)
      io.lambda.valid             #= false
      io.lambda.payload.foreach(_ #= 0)
      clockDomain.waitSampling()

      Seq(7, 7, 9).reverse.zip(io.lambda.payload).foreach { case (d, p) => p #= d }
      io.lambda.valid #= true
      clockDomain.waitSampling()
      io.lambda.valid #= false
      clockDomain.waitSampling(15)

      val gf16 = RS(15, 11)
      Range(0, 1000).foreach { i =>
        val data = Seq.fill(3)(nextInt((1 << 4) - 1) + 1) // [1, 15]
        io.lambda.payload.zip(data.reverse).foreach { case (p, d) => p #= d }
        io.lambda.valid #= true
        clockDomain.waitSampling()
        val golden = gf16.errorLocation(data)
        Range(1, 15).foreach { k =>
          clockDomain.waitSampling()
          val get = io.positionRLambda.payload(0).toInt
          assert(io.positionRLambda.valid.toBoolean == (golden(get) == 0))
        }

      }

    }

}
