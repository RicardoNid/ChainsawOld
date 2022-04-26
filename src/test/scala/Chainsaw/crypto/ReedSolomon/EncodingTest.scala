package Chainsaw.crypto.ReedSolomon

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.sim._
import spinal.core._
import spinal.lib._
import scala.util.Random._

class EncodingTest extends AnyFlatSpec {
  "RS(15, 11)" should "work normally" in SimConfig.withFstWave
    .withConfig(
      SpinalConfig(
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
        defaultClockDomainFrequency  = FixedFrequency(100 MHz)
      )
    )
    .compile(new Encoding(15, 11))
    .doSim { dut =>
      import dut._
      println(dut.coff.mkString(" "))
      val rs1511 = RS(15, 11)
      clockDomain.forkStimulus(10)
      io.message.payload #= 0
      io.message.valid   #= false
      clockDomain.waitSampling()

      Range(0, 1000).foreach { i =>
        val get    = Array.fill(15)(0)
        val data   = Seq.fill(11)(nextInt(1 << 4))
        val golden = rs1511.encode(data)
        var k      = 0
        while (k < 11) {
          val valid = nextBoolean()
          io.message.valid #= valid
          if (valid) {
            val dataArr = data.toArray
            io.message.payload #= dataArr(k)
          } else
            io.message.payload.randomize()
          clockDomain.waitSampling()
          if (valid) {
            get(k) = data(k)
            k += 1
          }
        }

        Range(11, 15).foreach { k =>
          io.message.valid.randomize()
          clockDomain.waitSampling()
          get(k) = io.output.payload.toInt
        }
        println("------------------------------------------------------")
        println("mess:   " + data.mkString(" "))
        println("get:    " + get.mkString(" "))
        println("golden: " + golden.mkString(" "))
        get.zip(golden).foreach { case (g, e) => assert(g == e) }
      }

      Range(1, 12).foreach { i =>
        io.message.payload #= i
        io.message.valid   #= true
        clockDomain.waitSampling()
      }
      io.message.payload #= 0
      io.message.valid   #= false
      clockDomain.waitSampling(6)
    }
}
