package Chainsaw.crypto.ReedSolomon

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._

class SyndromesCalcTest extends AnyFlatSpec {
  "RS(15, 11) syndrome calculator" should "work normally" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new SyndromesCalc(15, 11))
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(10)
      io.r.valid   #= false
      io.r.payload #= 0
      clockDomain.waitSampling()
      val rx = Seq(1, 2, 3, 4, 5, 11, 7, 8, 9, 10, 11, 3, 1, 12, 12)
      rx.foreach { i =>
        io.r.valid   #= true
        io.r.payload #= i
        clockDomain.waitSampling()
      }
      io.r.valid #= false
      io.r.payload.randomize()
      clockDomain.waitSampling(5)
      val sx   = Array.fill(4)(0)
      val gf16 = GaloisField(4)
      Range(0, 10000).foreach { i =>
        io.r.randomize()
        clockDomain.waitSampling()
        if (io.s.valid.toBoolean) {
          println(sx.mkString(" "))
          io.s.payload.map(_.toInt).zip(sx).foreach { case (p, s) => assert(p == s) }
          if (io.r.valid.toBoolean) {
            sx.indices.foreach(k => sx(k) = io.r.payload.toInt)
          } else {
            sx.indices.foreach(k => sx(k) = 0)
          }
        } else if (io.r.valid.toBoolean) {
          io.s.payload.map(_.toInt).zipWithIndex.foreach { case (p, i) => sx(i) = gf16.multi(sx(i), gf16.alphaPowTable(i)) ^ io.r.payload.toInt }
        }
      }

    }
}
