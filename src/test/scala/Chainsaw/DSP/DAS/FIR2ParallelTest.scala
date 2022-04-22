package Chainsaw.DSP.DAS

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core.sim._
import spinal.sim._
import spinal.core._
import spinal.lib._

import scala.collection.mutable
import scala.util.Random._
import scala.collection.mutable._
import scala.math._

class FIR2ParallelTest extends AnyFlatSpec {

  val coffSize = nextInt(100) + 5
  val coff     = Seq.fill(if (coffSize % 2 == 1) coffSize + 1 else coffSize)(nextDouble() * 5)
  s"${coffSize} order FIR with the ${coff.head}" should "work normally" in simNow(coff)
//  it should "work normally" in simNow(Seq(1, 2, 3, 4))

  def simNow(C: Seq[Double]) = {
    val data = Seq.fill(1000)(nextDouble() * 6 - 3)
    SimConfig.withFstWave
      .withConfig(
        SpinalConfig(
          defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
          defaultClockDomainFrequency  = FixedFrequency(100 MHz)
        )
      )
      .compile {
        val dut = new FIR2Parallel(C)
        dut.H0.coffSFix.foreach(_.simPublic())
        dut.H1.coffSFix.foreach(_.simPublic())
        dut
      }
      .doSim { dut =>
        import dut._
        val x = mutable.Queue[Double]()
        C.indices.init.foreach(i => x.enqueue(0.0))
        clockDomain.forkStimulus(10)
        io.dataIn.payload.foreach(_.raw #= 0)
        io.dataIn.valid                 #= false
        clockDomain.waitSampling()
        var validCount = 0
        var errCount   = 0
        val coffSF = H0.coffSFix.map(_.toDouble).zip(H1.coffSFix.map(_.toDouble)).map{case(z,o) => Seq(z, o)}.reduce(_ ++ _)
        println(coffSF.mkString(" "))
        Range(0, 10000).foreach { i =>
          io.dataIn.valid.randomize()
          io.dataIn.payload.foreach(_ #= nextDouble() * 6 - 3)
          clockDomain.waitSampling()
          if (io.dataIn.valid.toBoolean) {
            validCount += 1
            val d0 = io.dataIn.payload(0).toDouble
            val d1 = io.dataIn.payload(1).toDouble
            x.enqueue(d0)
            val f0 = x.zip(C).map { case (i, j) => i * j }.sum
            x.dequeue()
            x.enqueue(d1)
            val f1 = x.zip(C).map { case (i, j) => i * j }.sum
            x.dequeue()
            val p = io.dataOut.payload.map(_.toDouble)
            if (abs((f0 - p(0)) / f0) > 0.05 || abs((f1 - p(1)) / f1) > 0.05) {
              println("--------------------------------------")
              io.dataOut.payload.foreach(o => println(o.toDouble))
              println(f0)
              println(f1)
              errCount += 1
              println("err!!!!!!!!!!!!!")
            }
          }

        }
        println(validCount)
        println(errCount)
      }

  }
}
