package DSP

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable

//trait DSPSim extends DSPGen {
trait DSPSim extends Component {

  type TestCase
  type ResultType

  def messageWhenInvalid(refResult: ResultType, dutResult: ResultType): String = s"\n${refResult}\n${dutResult}"

  val testCases = mutable.Queue[TestCase]()
  val refResults = mutable.Queue[ResultType]()
  val dutResults = mutable.Queue[ResultType]()
  val exitPeriod = 10

  //  input.valid #= false
  def simInit(): Unit = {
    clockDomain.forkStimulus(2)
    clockDomain.waitSampling(10)
  }

  def simDone(): Unit = {
    clockDomain.waitSampling(10)
    while (refResults.nonEmpty || dutResults.nonEmpty) clockDomain.waitSampling(exitPeriod)
  }

  /** The function that takes the testCase and return the ground truth
   *
   * @param testCase
   * @return
   */
  def referenceModel(testCase: TestCase): ResultType

  /** Define the conditon by which you regard ref and dut as the same
   *
   * @param refResult
   * @param dutResult
   * @return
   */
  def isValid(refResult: ResultType, dutResult: ResultType): Boolean

  /** Define when and how the testCase is passed to DUT and reference model
   *
   * @example
   * val drv = fork {
   * while (true) {
   * if (testCases.nonEmpty) {
   * val testCase = testCases.dequeue()
   * input.valid #= true
   * input.payload #= testCase
   * val refResult = referenceModel(testCase)
   * printlnWhenDebug(s"time $simTime: refResult = $refResult")
   * refResults.enqueue(refResult)
   * clockDomain.waitSampling()
   * input.valid #= false
   * }
   * else clockDomain.waitSampling()
   * }
   * }
   */
  def driver()

  /**
   * example:
   * val mon = fork {
   * while (true) {
   * if (output.valid.toBoolean) {
   * val dutResult = output.payload.toInt
   * printlnWhenDebug(s"time $simTime: dutResult = $dutResult")
   * dutResults.enqueue(dutResult)
   * }
   * clockDomain.waitSampling()
   * }
   */
  def monitor()

  /** Compare ref and dut results, do assertion under test mode, and print the results under debug mode
   *
   */
  def scoreBoard(): Unit = {
    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val refResult = refResults.dequeue()
          val dutResult = dutResults.dequeue()
          if (!isValid(refResult, dutResult)) {
            printlnWhenDebug(Console.RED)
            printlnWhenDebug(messageWhenInvalid(refResult, dutResult))
            printlnWhenDebug(Console.BLACK)
          }
          assert(isValid(refResult, dutResult) || debug, messageWhenInvalid(refResult, dutResult))
        }
        clockDomain.waitSampling()
      }
    }
  }


  def insertTestCase(testCase: TestCase) = testCases.enqueue(testCase)

  def sim(): Unit = {
    simInit()
    driver()
    monitor()
    scoreBoard()
  }
}