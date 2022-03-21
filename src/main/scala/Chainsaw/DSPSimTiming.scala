package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable

trait DSPSimTiming[inputType <: Data, outputType <: Data, testCaseType, testResultType]
    extends DSPDUTTiming[inputType, outputType]
    with DSPSim[inputType, outputType, testCaseType, testResultType] {

  val monitorPoints = mutable.Queue[Long]()

  def simInit(): Unit = {
    clockDomain.forkStimulus(period)
    sleep(32)
  }

  /** Define when and how the testCase is passed to the DUT and the reference model
    */
  def driver(): Unit = {
    fork {
      while (true) {
        if (testCases.nonEmpty) {
          monitorPoints.enqueue(simCycle + timing.latency + 1)
          val testCase = testCases.dequeue()
          lastCase.enqueue(testCase)
          val refResult = referenceModel(testCase)
          refResults.enqueue(refResult)
          poke(testCase, input)
          clockDomain.waitSampling() // input interval >= 1
          clockDomain.waitSampling(timing.initiationInterval - timing.inputInterval)
        } else clockDomain.waitSampling()
      }
    }
  }

  /** Define when and how the testResult is fetched from the DUT
    */
  def monitor(): Unit = {
    fork {
      while (true) {
        if (monitorPoints.nonEmpty && simCycle == monitorPoints.head) {
          monitorPoints.dequeue()
          val dutResult = peek(output)
          dutResults.enqueue(dutResult)
          clockDomain.waitSampling() // output interval >= 1
        } else clockDomain.waitSampling()
      }
    }
  }

  /** Thread that terminates the simulation
    *
    * @return
    *   The report of simulation
    */
  def simDone(): SimReport = {
    clockDomain.waitSampling(2) // start checking at the time right after when first case is dequeued
    // the time needed at most to finish a single testCase
    val protect = timing.latency + timing.outputInterval + 5
    while (testCases.nonEmpty) {
      clockDomain.waitSampling(timing.initiationInterval)
    }
    clockDomain.waitSampling(protect)
    SimReport(trueCase, totalCase, log, validLog)
  }
}
