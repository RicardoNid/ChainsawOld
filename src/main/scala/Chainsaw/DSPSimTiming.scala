package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable

trait DSPSimTiming[inputType <: Data, outputType <: Data, testCaseType, testResultType]
  extends DSPSim[inputType, outputType, testCaseType, testResultType] with DSPDUTTiming[inputType, outputType] {

  val monitorPoints = mutable.Queue[Long]()

  def simInit(): Unit = {
    clockDomain.forkStimulus(period)
    //    input.valid #= false
    clockDomain.waitSampling(10)
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
          //          input.valid #= true
          //          poke(testCase, input.payload)
          poke(testCase, input)
          clockDomain.waitSampling() // input interval >= 1
          //          input.valid #= false
          clockDomain.waitSampling(timing.initiationInterval - timing.inputInterval)
        }
        else clockDomain.waitSampling()
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
          //          val dutResult = peek(output.payload)
          val dutResult = peek(output)
          dutResults.enqueue(dutResult)
          clockDomain.waitSampling() // output interval >= 1
        }
        else clockDomain.waitSampling()
      }
    }
  }

  /** Thread that terminates the simulation, if no result was generated during the last protect period
   *
   * @return The report of simulation
   */
  def simDone(): SimReport = {
    clockDomain.waitSampling(10)
    val protect = timing.initiationInterval - timing.latency - timing.outputInterval
    while (refResults.nonEmpty || dutResults.nonEmpty) clockDomain.waitSampling(if (protect > 0) protect else 1)
    SimReport(trueCase, totalCase, log, validLog)
  }
}

