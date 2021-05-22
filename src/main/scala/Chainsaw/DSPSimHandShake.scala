package Chainsaw

import spinal.core.Data
import spinal.core.sim.{fork, _}

@unchecked // TODO: develop this in the futer
trait DSPSimHandShake[inputType <: Data, outputType <: Data, testCaseType, testResultType] extends
  DSPDUTHandShake[inputType, outputType] with DSPSim[inputType, outputType, testCaseType, testResultType] {

  def simInit(): Unit = {
    clockDomain.forkStimulus(period)
    input.valid #= false
    output.ready #= true
    clockDomain.waitSampling(10)
  }

  /** Define when and how the testCase is passed to the DUT and the reference model
   */
  def driver(): Unit = {
    fork {
      while (true) {
        if (testCases.nonEmpty && input.ready.toBoolean) {
          {
            val testCase = testCases.dequeue()
            lastCase.enqueue(testCase)
            val refResult = referenceModel(testCase)
            refResults.enqueue(refResult)
            input.valid #= true
            poke(testCase, input.payload)
            clockDomain.waitSampling()
            input.valid #= false
          }
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
        if (output.valid.toBoolean) {
          val dutResult = peek(output.payload)
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
    // the refResult would be generated as soon as possible
    while (refResults.nonEmpty || dutResults.nonEmpty) clockDomain.waitSampling(5)
    SimReport(trueCase, totalCase, log, validLog)
  }

}
