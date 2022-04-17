package Chainsaw.DSP.DAS
import Chainsaw._

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import scala.collection.mutable._
import scala.util.Random
import scala.math._

class FIRTest extends AnyFlatSpec {
  Range(0, 10).foreach { i =>
    val coffLength = Random.nextInt(100) + 4
    val coff       = Seq.fill(coffLength)(Random.nextDouble() * 10 - 5)
    s"test: ${i}: ${coffLength} order FIR" should "work normally " in simNow(coff)

  }

  def simNow(coff: Seq[Double]): Unit = {
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .withCoverage
      .compile(new FIR(coff))
      .doSim { dut =>
        import dut._
        val x = Queue[Double]()
        coff.indices.init.foreach(i => x.enqueue(0))
        clockDomain.forkStimulus(10)
        io.dataIn.valid       #= false
        io.dataIn.payload.raw #= 0
        clockDomain.waitSampling()
        println(coff.mkString(" "))
        Range(0, 1000).foreach { i =>
          io.dataIn.payload.raw #= Random.nextInt(1 << 10) - Random.nextInt(1 << 5)
          io.dataIn.valid.randomize()
          clockDomain.waitSampling()
          if (io.dataIn.valid.toBoolean) {
            println("-----------------------------------------------")
            x.enqueue(io.dataIn.payload.toDouble)
            val gold = x.zip(coff).map { case (i, j) => i * j }.sum
            println(gold)
            println(io.dataOut.payload.toDouble)
            val err = abs(io.dataOut.payload.toDouble - gold) / abs(gold)
            println(err)
            if (gold > 5)
              assert(err < 0.1)
            x.dequeue()
          }
        }

      }
  }

}
