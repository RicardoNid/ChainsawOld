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

class AngleCordicTest extends AnyFlatSpec {
//  VivadoFlow(AngleCordic(), "AngleCordic", Chainsaw.synthWorkspace).doit()
//  new QuartusFlow(AngleCordic()).impl()
  "the phase got by cordic" should "work normally" in
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile(new AngleCordic())
      .doSim { dut =>
        import dut._
        val queue = Queue[(Double, Double)]()
        var count = 0
        clockDomain.forkStimulus(10)
        io.yIn.raw #= 0
        io.xIn.raw #= 0
        clockDomain.waitSampling()

        Range(0, 1000).foreach { i =>
          //TODO x range and y range
          io.yIn #= Chainsaw.ChainsawRand.nextDouble() * (6.2 * 2) / 2 //(-6.2, 6.2)
          io.xIn #= Chainsaw.ChainsawRand.nextDouble() * (6.2 * 2) / 2 //(-6.2, 6.2)
          clockDomain.waitSampling()
          queue.enqueue((io.yIn.toDouble, io.xIn.toDouble))
          if (count <= 15) {
            count = count + 1
          } else {
            println("--------------------------")
            val Get    = queue.dequeue()
            val golden = atan(Get._1 / Get._2)
            println(s"${golden} = arctan(${Get._1} / ${Get._2})")
            println(io.zOut.toDouble)
            assert(abs(golden - io.zOut.toDouble) < 0.03)
          }
        }
      }
}
