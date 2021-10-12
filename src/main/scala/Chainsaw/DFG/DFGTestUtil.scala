package Chainsaw.DFG

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

object DFGTestUtil {

  /** Verify that the transformed DFG has the same function as the original one
   *
   * @param original
   * @param transformed
   * @param speedUp the throughput of transformed DFG, 3 for *3, -3 for 1/3
   * @param delay   extra delay on latency, latency' = latency / speedUp + delayed
   * @tparam T
   */
  def verifyFunction[T <: BitVector](original: DFGGraph[T], transformed: DFGGraph[T], elementType: HardType[T], speedUp: Int, delay: Int, testLength: Int = 50) = {

    if (speedUp > 1) { // throughput > 1, bigger port number
      require(transformed.inputNodes.size == original.inputNodes.size * speedUp)
      require(transformed.outputNodes.size == original.outputNodes.size * speedUp)
    } else if (speedUp < -1) { // throughput < 1, keep the port number
      require(transformed.inputNodes.size == original.inputNodes.size)
      require(transformed.outputNodes.size == original.outputNodes.size)
    } else throw new IllegalArgumentException()

    val testCases = ArrayBuffer[BigInt]()
    val results = ArrayBuffer[BigInt]()
    val transFormedTestCases = ArrayBuffer[BigInt]()
    val transFormedResults = ArrayBuffer[BigInt]()

    // get data from the original
    SimConfig.withWave.compile(new Component {
      val dataIn = slave Flow Vec(elementType, original.inputNodes.size)
      val dataOut = master Flow Vec(elementType, original.outputNodes.size)
      dataOut.payload := Vec(original.impl(dataIn.payload))
      dataOut.valid := Delay(dataIn.valid, original.latency, init = False)
    }).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}

      dataIn.halt()
      dataIn.payload.foreach(_ #= 0)
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
      dataIn.setMonitor(testCases) // FIXME: one extra sampling on the first input, why?
      dataOut.setMonitor(results)

      (0 until testLength).foreach { testCase =>
        dataIn.pokeRandom()
        clockDomain.waitSampling()
      }
      clockDomain.waitSampling(original.latency)
    }

    val inputSchedule = transformed.outgoingEdgesOf(transformed.inputNodes.head).head.schedules.head
    val outputSchedule = transformed.incomingEdgesOf(transformed.outputNodes.head).head.schedules.head

    // TODO: make it correct
    val transformedLatency = if (speedUp < 0) {
      (original.latency * -speedUp) + (outputSchedule.time * transformed.globalLcm / outputSchedule.period) - (inputSchedule.time * transformed.globalLcm / inputSchedule.period)
    } else {
      original.latency
    } + delay

    // using the same data on the transformed dfg
    SimConfig
      .withWave.compile(new Component {
      val dataIn = slave Flow Vec(elementType, transformed.inputNodes.size)
      val dataOut = master Flow Vec(elementType, transformed.outputNodes.size)
      dataOut.payload := Vec(transformed.impl(dataIn.payload))
      dataOut.valid := Delay(dataIn.valid, transformedLatency, init = False)
    }).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}

      dataIn.halt()
      dataIn.payload.foreach(_ #= 0)
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
      clockDomain.waitSampling((inputSchedule.time - 1) * transformed.globalLcm / inputSchedule.period)
      clockDomain.waitSampling(transformed.globalLcm)
      dataIn.setMonitor(transFormedTestCases)
      dataOut.setMonitor(transFormedResults)

      if (speedUp < 0) {
        testCases.tail.grouped(original.inputNodes.size).toSeq.foreach { testCase =>
          dataIn.poke(testCase)
          clockDomain.waitSampling()
          dataIn.halt()
          clockDomain.waitSampling(-speedUp - 1)
        }
      } else {
        testCases.tail.grouped(transformed.inputNodes.size).toSeq.foreach { testCase =>
          dataIn.poke(testCase)
          clockDomain.waitSampling()
        }
      }

      clockDomain.waitSampling(transformedLatency)
    }

    println(s"input to the original ${testCases.mkString(" ")}")
    println(s"input to the transformed ${transFormedTestCases.mkString(" ")}")
    printlnGreen(results.mkString(" "))
    printlnGreen(transFormedResults.mkString(" "))
    printlnGreen(results.diff(transFormedResults))
    assert(results.zip(transFormedResults).forall { case (ori, trans) => ori == trans })

  }
}
