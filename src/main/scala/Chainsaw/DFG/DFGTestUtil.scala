package Chainsaw.DFG

import Chainsaw._
import Chainsaw.dspTest._
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


object DFGTestUtil {

  /** Verify that the transformed DFG has the same function as the original one
   *
   * @param original    DFG before transformation
   * @param transformed DFG after transformation
   * @param speedUp     the throughput of transformed DFG, 3 for *3, -3 for 1/3
   * @param delay       extra delay on latency, latency' = latency / speedUp + delayed
   */
  def verifyFunctionalConsistency[T <: Data](original: DFGGraph[T], transformed: DFGGraph[T],
                                             elementType: HardType[T], speedUp: Int, latencyTransformations: Seq[LatencyTrans], testLength: Int = 50,
                                             name: String = null, basicLatency: Int = -1)(implicit holderProvider: BitCount => T): Unit = {

    val logger = LoggerFactory.getLogger("FunctionalConsistencyLogger")

    // requirement on the input/output size
    val scaleFactor = if (speedUp > 1) speedUp else 1
    require(transformed.inputNodes.size == original.inputNodes.size * scaleFactor &&
      transformed.outputNodes.size == original.outputNodes.size * scaleFactor,
      "speed up factor doesn't match the port number")

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

    logger.info(s"input at $inputSchedule, output at $outputSchedule")

    val originalLatency = if (basicLatency == -1) {original.latency} else basicLatency
    var transformedLatency = originalLatency
    latencyTransformations.foreach(trans => transformedLatency = trans.trans(transformedLatency))

    /** Describe the simulation procedure
     *
     * @param dfg
     * @param latency
     * @param speedUp
     * @param inputRecord
     * @param outputRecord
     * @param testCases
     */
    def testDFG(dfg: DFGGraph[T], latency: Int, speedUp: Int,
                inputRecord: ArrayBuffer[BigInt], outputRecord: ArrayBuffer[BigInt],
                testCases: ArrayBuffer[BigInt] = null) = {

      //      testDSPNode(dfg.asNode("temp", latencies cycles,dataReset = true))

      SimConfig.withWave
        .workspaceName(name)
        .compile(new Component {
          val dataIn = slave Flow Vec(elementType, dfg.inputNodes.size)
          val dataOut = master Flow Vec(elementType, dfg.outputNodes.size)
          dataOut.payload := Vec(dfg.impl(dataIn.payload, dataReset = true)).resized
          dataOut.valid := Delay(dataIn.valid, latency, init = False)
        }).doSim { dut =>
        import dut.{clockDomain, dataIn, dataOut}

        implicit val currentDFG = dfg
        val inputSchedule = dfg.inputNodes.head.outgoingEdges.head.schedules.head

        dataIn.halt() // TODO: halt should also set zero on the payload
        // TEMP: simplify the initialization
        dataIn.payload.foreach(signal => signal match {
          case uint: UInt => uint #= 0
          case sint: SInt => sint #= 0
        })

        //        dataIn.payload.randomize()
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
              // TODO: clarify this part
              val forUnfolded = DSP.interleave.Algos.matIntrlv(testCase, speedUp, testCase.size / speedUp)
              dataIn.poke(forUnfolded)
              clockDomain.waitSampling()
            }
          }
        }
        clockDomain.waitSampling(latency)
      }
    }

    testDFG(original, originalLatency, 1, originalTestCases, originalResults)
    testDFG(transformed, transformedLatency, speedUp, transFormedTestCases, transFormedResults, originalTestCases)

    logger.info(s"verification result:" +
      s"\ninput to the original    ${originalTestCases.mkString(" ")}" +
      s"\ninput to the transformed ${transFormedTestCases.mkString(" ")}" +
      s"\noriginal result    ${originalResults.mkString(" ")}" +
      s"\ntransformed result ${transFormedResults.mkString(" ")}" +
      s"\ndiff part          ${originalResults.diff(transFormedResults).mkString(" ")}")

    assert(originalResults.size > 10)
    assert(transFormedResults.size > 10)
    assert(originalResults.zip(transFormedResults).forall { case (ori, trans) => ori == trans }) // FIXME: verify the unaligned part
  }

  def verifyFolding[T <: Data](original: DFGGraph[T], foldingSets: Seq[Seq[DSPNode[T] with Foldable[T]]], elementType: HardType[T], name: String = null, basicLatency: Int = -1)
                              (implicit holderProvider: BitCount => T) = {
    val algo = new Folding(original, foldingSets)
    val foldedDFG = algo.folded
    val N = foldingSets.head.size
    verifyFunctionalConsistency(original, foldedDFG, elementType, -N, algo.latencyTransformations, name = name, basicLatency = basicLatency) // TODO: customized width
  }

  def verifyUnfolding[T <: Data](original: DFGGraph[T], unfoldingFactor: Int, elementType: HardType[T], name: String = null, basicLatency: Int = -1)
                                (implicit holderProvider: BitCount => T) = {
    val algo = new Unfolding(original, unfoldingFactor)
    val unfoldedDFG = algo.unfolded
    verifyFunctionalConsistency(original, unfoldedDFG, elementType, unfoldingFactor, algo.latencyTransformations, name = name, basicLatency = basicLatency)
  }

}
