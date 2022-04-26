package Chainsaw.crypto.ReedSolomon

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.sim._
import spinal.core._
import spinal.lib._

import scala.util.Random._
import GaloisFieldHardWare._
import spinal.core

class EuclideanTest extends AnyFlatSpec {
  val gf16 = RS(15, 11)

  SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new Euclidean())
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(10)
      io.S.valid                #= false
      io.gammaLambdaOmega.ready #= false
      io.S.payload.foreach(_    #= 0)
      clockDomain.waitSampling()
      Range(0, 1000).foreach { i =>
        println("-------------------------------------")
        val data = Seq.fill(4)(nextInt((1 << 4) - 1) + 1) // h -> l
        println(data.mkString(" "))
        data.reverse.zip(io.S.payload).foreach { case (s, p) => p #= s }
        io.S.valid #= true
        clockDomain.waitSampling()
        io.S.valid #= false
        while (!io.gammaLambdaOmega.valid.toBoolean) {
          clockDomain.waitSampling()
        }
        io.gammaLambdaOmega.ready #= true
        clockDomain.waitSampling(2)
      }

    }
}
