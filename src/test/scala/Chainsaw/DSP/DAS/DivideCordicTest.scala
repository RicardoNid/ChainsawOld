package Chainsaw.DSP.DAS

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import xilinx.VivadoFlow
import Chainsaw._
import scala.collection.mutable.Queue
import scala.math._

class DivideCordicTest extends AnyFlatSpec {
//  VivadoFlow(DivideCordic(), "DivideCordic", Chainsaw.synthWorkspace).doit()
  "the cordic divder" should "work normally" in
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile(new DivideCordic())
      .doSim { dut =>
        import dut._
        val queue = Queue[Double]()
        var count = 0
        clockDomain.forkStimulus(10)
        yIn.raw #= 0
        clockDomain.waitSampling()
        Range(0, 100).foreach { i =>
          yIn #= Chainsaw.ChainsawRand.nextDouble() * (6.2 * 2) / 2 //(-6.2, 6.2)
          clockDomain.waitSampling()
          queue.enqueue(yIn.toDouble)
          if (count <= 15) {
            count = count + 1
          } else {
            println("--------------------------")
            val yGet   = queue.dequeue()
            val golden = yGet / 3.140625
            println(yGet)
            println(golden)
            println(zOut.toDouble)
            assert(abs(golden - zOut.toDouble) < 0.01)
          }
        }
      }
}
