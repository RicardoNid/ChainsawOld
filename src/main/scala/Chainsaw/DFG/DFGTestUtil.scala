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
   * @param original    DFG before transformation
   * @param transformed DFG after transformation
   * @param speedUp     the throughput of transformed DFG, 3 for *3, -3 for 1/3
   * @param delay       extra delay on latency, latency' = latency / speedUp + delayed
   * @tparam T
   */
  def verifyFunctionalConsistency[T <: BitVector](original: DFGGraph[T], transformed: DFGGraph[T], elementType: HardType[T], speedUp: Int, delay: Int, testLength: Int = 50) = {

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
    val transformedLatency = if (speedUp < 0) {
      (original.latency * -speedUp) + outputSchedule.time - inputSchedule.time
    } else {
      original.latency
    } + delay

    def testDFG(dfg: DFGGraph[T], latency: Int, speedUp: Int,
                inputRecord: ArrayBuffer[BigInt], outputRecord: ArrayBuffer[BigInt],
                testCases: ArrayBuffer[BigInt] = null) = {
      SimConfig.withWave.compile(new Component {
        val dataIn = slave Flow Vec(elementType, dfg.inputNodes.size)
        val dataOut = master Flow Vec(elementType, dfg.outputNodes.size)
        dataOut.payload := Vec(dfg.impl(dataIn.payload))
        dataOut.valid := Delay(dataIn.valid, latency, init = False)
      }).doSim { dut =>
        import dut.{clockDomain, dataIn, dataOut}

        implicit val currentDFG = dfg
        val inputSchedule = dfg.inputNodes.head.outgoingEdges.head.schedules.head

        dataIn.halt()
        dataIn.payload.foreach(_ #= 0)
        // FIXME: solve sampling problem by
        dataIn.setMonitor(inputRecord, "input")
        clockDomain.forkStimulus(2)
        clockDomain.waitSampling()
        if(speedUp > 1) clockDomain.waitSampling() // FIXME: specially for mux unfolding
        clockDomain.waitSampling((inputSchedule.time - 1)  + transformed.globalLcm)
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
            testCases.grouped(transformed.inputNodes.size).toSeq.foreach { testCase =>
              // folding would change the order of input
              val forUnfolded = DSP.interleave.Algos.matIntrlv(testCase, speedUp, testCase.size / speedUp)
              dataIn.poke(forUnfolded)
              clockDomain.waitSampling()
            }
          }
        }
        clockDomain.waitSampling(latency)
      }
    }

    testDFG(original, original.latency, 1, originalTestCases, originalResults)
    testDFG(transformed, transformedLatency, speedUp, transFormedTestCases, transFormedResults, originalTestCases)

    println(s"input to the original ${originalTestCases.mkString(" ")}")
    println(s"input to the transformed ${transFormedTestCases.mkString(" ")}")
    printlnGreen(originalResults.mkString(" "))
    printlnGreen(transFormedResults.mkString(" "))
    printlnGreen(originalResults.diff(transFormedResults))

    assert(originalResults.dropWhile(_ == 0).size > 10)
    assert(transFormedResults.dropWhile(_ == 0).size > 10)
    // FIXME: this is a test after shifting, finally, you should implement a test with exact timing
    assert(originalResults.dropWhile(_ == 0).zip(transFormedResults.dropWhile(_ == 0)).forall { case (ori, trans) => ori == trans })
  }

  def verifyFolding(original: DFGGraph[SInt], foldingSets: Seq[Seq[DSPNode[SInt] with Foldable[SInt]]]) = {
    val foldedDFG = new Folding(original, foldingSets).folded
    val N = foldingSets.head.size
    verifyFunctionalConsistency(original, foldedDFG, HardType(SInt(10 bits)), -N, 0)
  }

}
