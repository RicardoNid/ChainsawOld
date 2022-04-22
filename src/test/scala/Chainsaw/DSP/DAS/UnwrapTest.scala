package Chainsaw.DSP.DAS

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.sim._
import scala.util.Random
import Chainsaw._
import Para._
import scala.math._
import spinal.core._
import spinal.lib._

class UnwrapTest extends AnyFlatSpec {
//  new QuartusFlow(Unwrap(128)).impl()
  it should "work normally " in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile {
      val dut = Unwrap(256)
      dut.xn1.simPublic()
      dut.colRAM.addAttribute("ram_style", "block")
      dut
    }
    .doSim { dut =>
      import dut._
      val colRAM = scala.collection.mutable.Queue[Double]()
      clockDomain.forkStimulus(10)
      io.xn.payload.raw #= 0
      io.xn.valid       #= false
      clockDomain.waitSampling()
      Range(0, 1000).foreach { i =>
        io.xn.payload.raw #= ChainsawRand.nextBigInt(peakExp + resolutionExp - 1)
        io.xn.valid.randomize()
        clockDomain.waitSampling()

        if (io.xn.valid.toBoolean) {
          println("-----------------------------------------------")
          val xn    = io.xn.payload.toDouble
          val xnOut = io.xnOut.payload.toDouble
          println("xn: " + xn)
          println("xnOut: " + xnOut)
          if (io.xnOut.valid.toBoolean) {
            colRAM.enqueue(xnOut)
            val xn1q = colRAM.dequeue()
            val xn1  = dut.xn1.toDouble
            println("xn1q: " + xn1q)
            println("xn1: " + xn1)
            assert(abs(xnOut - xn1q) <= 1)
          } else (colRAM.enqueue(xn))
        }
      }
    }
}
