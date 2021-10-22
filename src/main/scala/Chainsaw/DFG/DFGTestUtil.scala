package Chainsaw.DFG

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import org.slf4j.{Logger, LoggerFactory}


object DFGTestUtil {

  /** Verify that the transformed DFG has the same function as the original one
   *
   * @param original    DFG before transformation
   * @param transformed DFG after transformation
   * @param speedUp     the throughput of transformed DFG, 3 for *3, -3 for 1/3
   * @param delay       extra delay on latency, latency' = latency / speedUp + delayed
   * @tparam SInt
   */
  def verifyFunctionalConsistency(original: DFGGraph[SInt], transformed: DFGGraph[SInt],
                                  elementType: HardType[SInt], speedUp: Int, delay: Int, testLength: Int = 50,
                                  name: String = null) = {

    val logger = LoggerFactory.getLogger("FunctionalConsistencyLogger")

    // requirement on the input/output size
    if (speedUp > 1) { // throughput > 1, bigger port number
      require(transformed.inputNodes.size == original.inputNodes.size * speedUp)
      require(transformed.outputNodes.size == original.outputNodes.size * speedUp)
    } else if (speedUp < -1) { // throughput < 1, keep the port number
      require(transformed.inputNodes.size == original.inputNodes.size)
      require(transformed.outputNodes.size == original.outputNodes.size)
    } else throw new IllegalArgumentException("speed up factor doesn't match the port number")

    // data tobe filled
    val originalTestCases = ArrayBuffer[BigInt]()
    val originalResults = ArrayBuffer[BigInt]()
    val transFormedTestCases = ArrayBuffer[BigInt]()
    val transFormedResults = ArrayBuffer[BigInt]()

    // FIXME: this only works for SISO / homogeneous MIMO DFG
    // FIXME: find out the actual latency formula
    implicit val currentDFG = transformed
    val inputSchedule = transformed.inputNodes.head.outgoingEdges.head.schedules.head
    val outputSchedule = transformed.outputNodes.head.incomingEdges.head.schedules.head

    logger.info(s"\ninput should happen when counter value is $inputSchedule, output should happen when counter value is $outputSchedule")

    val transformedLatency =  (if (speedUp < 0) original.latency * -speedUp else original.latency / speedUp) + delay

    /** Describe the simulation procedure
     *
     * @param dfg
     * @param latencies
     * @param speedUp
     * @param inputRecord
     * @param outputRecord
     * @param testCases
     */
    def testDFG(dfg: DFGGraph[SInt], latencies: Int, speedUp: Int,
                inputRecord: ArrayBuffer[BigInt], outputRecord: ArrayBuffer[BigInt],
                testCases: ArrayBuffer[BigInt] = null) = {
      SimConfig.withWave
        .workspaceName(name)
        .compile(new Component {
          val dataIn = slave Flow Vec(elementType, dfg.inputNodes.size)
          val dataOut = master Flow Vec(elementType, dfg.outputNodes.size)
          dataOut.payload := Vec(dfg.impl(dataIn.payload))
          dataOut.valid := Delay(dataIn.valid, latencies, init = False)
        }).doSim { dut =>
        import dut.{clockDomain, dataIn, dataOut}

        implicit val currentDFG = dfg
        val inputSchedule = dfg.inputNodes.head.outgoingEdges.head.schedules.head

        dataIn.halt() // TODO: halt should also set zero on the payload
        dataIn.payload.foreach(_ #= 0)
        // FIXME: solve sampling problem by
        dataIn.setMonitor(inputRecord, "input")
        clockDomain.forkStimulus(2)
        clockDomain.waitSampling() // now, the global counter value is 1
        clockDomain.waitSampling((inputSchedule.time - 1) + dfg.globalLcm) // to align the counter value with input schedule
        dataOut.setMonitor(outputRecord, "output")

        if (testCases == null) {
          (0 until testLength).foreach { _ =>
            dataIn.pokeRandom()
            clockDomain.waitSampling()
          }
        }
        else {

          if (speedUp < 0) {
            testCases.grouped(original.inputNodes.size).toSeq.foreach { testCase =>
              dataIn.poke(testCase)
              clockDomain.waitSampling()
              dataIn.halt()
              clockDomain.waitSampling(-speedUp - 1)
            }
          } else {
            testCases.grouped(transformed.inputNodes.size).toSeq.init.foreach { testCase => // using init to drop the incomplete group
              // folding would change the order of input
              val forUnfolded = DSP.interleave.Algos.matIntrlv(testCase, speedUp, testCase.size / speedUp)
              dataIn.poke(forUnfolded)
              clockDomain.waitSampling()
            }
          }
        }
        clockDomain.waitSampling(latencies)
      }
    }

    testDFG(original, original.latency, 1, originalTestCases, originalResults)
    testDFG(transformed, transformedLatency, speedUp, transFormedTestCases, transFormedResults, originalTestCases)

    println(s"input to the original ${originalTestCases.mkString(" ")}")
    println(s"input to the transformed ${transFormedTestCases.mkString(" ")}")
    printlnGreen(originalResults.mkString(" "))
    printlnGreen(transFormedResults.mkString(" "))
    printlnGreen(originalResults.diff(transFormedResults))

    assert(originalResults.size > 10)
    assert(transFormedResults.size > 10)
    // FIXME: this is a test after shifting, finally, you should implement a test with exact timing
    assert(originalResults.zip(transFormedResults).forall { case (ori, trans) => ori == trans })
  }

  def verifyFolding(original: DFGGraph[SInt], foldingSets: Seq[Seq[DSPNode[SInt] with Foldable[SInt]]], name: String = null) = {
    val algo = new Folding(original, foldingSets)
    val foldedDFG = algo.folded
    val N = foldingSets.head.size
    verifyFunctionalConsistency(original, foldedDFG, HardType(SInt(10 bits)), -N, delay = algo.latencyTrans.shift, name = name)
  }

  def verifyUnfolding(original: DFGGraph[SInt], unfoldingFactor: Int, name: String = null) = {
    val foldedDFG = new Unfolding(original, unfoldingFactor).unfolded
    verifyFunctionalConsistency(original, foldedDFG, HardType(SInt(10 bits)), unfoldingFactor, 0, name = name)
  }

}
