package DSP

import scala.collection.mutable

trait DSPSim {

  type TestCase
  type ResultType

  val testCases = mutable.Queue[TestCase]()
  val refResults = mutable.Queue[ResultType]()
  val dutResults = mutable.Queue[ResultType]()

  def simInit()

  def simDone()

  def driver()

  def referenceModel(testCase: TestCase)

  def monitor()

  def scoreBoard()

  def insertTestCase(testCase: TestCase) = testCases.enqueue(testCase)

  def sim(): Unit = {
    simInit()
    driver()
    monitor()
    scoreBoard()
  }
}