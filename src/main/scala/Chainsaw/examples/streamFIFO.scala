package Chainsaw.examples

import Chainsaw._
import Chainsaw.dspTest._
import breeze.numerics._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

object streamFIFO {

  def main(args: Array[String]): Unit = {

    val width = 32

    SimConfig.withWave.compile(
      new Component {

        val domainWrite = ClockDomain.external("domainWrite", config = ClockDomainConfig(resetKind = ASYNC), frequency = FixedFrequency(50 MHz))
        val domainRead = ClockDomain.external("domainRead", config = ClockDomainConfig(resetKind = ASYNC), frequency = FixedFrequency(30 MHz))

        // out > in
        // burst = 300 -> depth = 300 - floor(300 / 0.5 * 0.3) = 120
        // BRAM address is a power of 2, nextPower2(120) = 128
        // 8 * 128 = 1024

        val fifo = StreamFifoCC(Bits(width bits), nextPower2(120).toInt, domainWrite, domainRead)

        val dataIn = slave Stream (Bits(width bits))
        val dataOut = master Stream (Bits(width bits))

        dataIn >> fifo.io.push
        fifo.io.pop >> dataOut
      }).doSim { dut =>

      dut.dataOut.ready #= false

      val dataInRecords = ArrayBuffer[BigInt]()
      val dataOutRecords = ArrayBuffer[BigInt]()

      dut.dataIn.setMonitor(dataInRecords)
      dut.dataOut.setMonitor(dataOutRecords)

      val clock500 = dut.domainWrite
      val clock300 = dut.domainRead
      clock300.forkStimulus(10)
      clock500.forkStimulus(6)
      clock500.waitSampling()
      clock300.waitSampling()

      dut.dataOut.ready #= true
      (0 until 500).foreach { i =>
        if (i < 300) {
          dut.dataIn.poke(BigInt(i % 256))
          clock500.waitSampling()
        }
        else {
          dut.dataIn.halt()
          clock500.waitSampling()
        }
      }

      clock500.waitSampling(500)
      println(dataInRecords.mkString(" "))
      println(dataOutRecords.mkString(" "))
    }

    VivadoSynth(new Component {

      val domainWrite = ClockDomain.external("domainWrite", config = ClockDomainConfig(resetKind = ASYNC), frequency = FixedFrequency(500 MHz))
      val domainRead = ClockDomain.external("domainRead", config = ClockDomainConfig(resetKind = ASYNC), frequency = FixedFrequency(300 MHz))

      // out > in
      // burst = 300 -> depth = 300 - floor(300 / 0.5 * 0.3) = 120
      // BRAM address is a power of 2

      val fifo = StreamFifoCC(Bits(width bits), nextPower2(120).toInt, domainWrite, domainRead)

      val dataIn = slave Stream Bits(width bits)
      val dataOut = master Stream Bits(width bits)

      dataIn >> fifo.io.push
      fifo.io.pop >> dataOut
    })
  }
}
