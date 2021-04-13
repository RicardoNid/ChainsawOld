package DSP

import spinal.core.sim._

import scala.io.Source
import scala.util.Random

class ShiftAdderGraphSim(adderGraph: AdderGraph) extends ShiftAdderGraphDUT(adderGraph) with DSPSim {
  override type TestCase = Double
  override type ResultType = Array[Double]


  override def simInit(): Unit = {
    input.valid #= false
    super.simInit()
  }

  /** The function that takes the testCase and return the ground truth
   *
   * @param testCase
   * @return
   */
  override def referenceModel(testCase: Double) = adderGraph.valueOfOutputs.toArray.sorted.map(_ * testCase)

  /** Define the conditon by which you regard ref and dut as the same
   *
   * @param refResult
   * @param dutResult
   * @return
   */
  override def isValid(refResult: Array[Double], dutResult: Array[Double]): Boolean = sameFixedSeq(refResult, dutResult)

  /** Define when and how the testCase is passed to DUT and reference model
   *
   * @example
   */
  override def driver(): Unit = {
    val drv = fork {
      while (true) {
        if (testCases.nonEmpty) {
          val testCase = testCases.dequeue()
          input.valid #= true
          input.payload.raw #= Double2Fix(testCase)
          val refResult = referenceModel(testCase)
          printlnWhenDebug(s"time $simTime: refResult = $refResult")
          refResults.enqueue(refResult)
          clockDomain.waitSampling()
          input.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  /**
   * example:
   */
  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (output.valid.toBoolean) {
          val dutResult = output.payload.map(Fix2Double(_)).toArray
          printlnWhenDebug(s"time $simTime: dutResult = $dutResult")
          dutResults.enqueue(dutResult)
        }
        clockDomain.waitSampling()
      }
    }
  }

  override def messageWhenInvalid(refResult: Array[Double], dutResult: Array[Double]): String = {
    refResult.zip(dutResult).filter { case (d0, d1) => !sameFixed(d0, d1) }.mkString(" ")
  }
}

object ShiftAdderGraphSim {
  private val r = Random

  def randomCase = randData()

  def randomSim(adderGraph: AdderGraph): Unit = {
    val dut = SimConfig.withWave.compile(new ShiftAdderGraphSim(adderGraph))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 100) dut.insertTestCase(randomCase)
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"shiftAdderGraph, PASSED")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {

    //    debug = true
    //    val adderGraph = new AdderGraph()
    //    adderGraph.addFundamental(1, 1, AOperation(2, SUBNEXT))
    //    adderGraph.addFundamental(1, 1, AOperation(3, ADD))
    //    adderGraph.addFundamental(9, 1, AOperation(0, 1, 0, ADD))
    //    adderGraph.addFundamental(9, 1, AOperation(0, 2, 0, ADD))
    //    adderGraph.addFundamental(3, 11, AOperation(0, 4, 0, SUBPREV))
    //    adderGraph.addOutput(9, 0)
    //    adderGraph.addOutput(11, 2, negation = true)
    //    adderGraph.addOutput(13, 4)
    //    adderGraph.addOutput(173, 1)
    //
    //    println(adderGraph.outputs.mkString(" "))
    //
    //    randomSim(adderGraph)

    val coe = Source.fromFile("ex2PM16_119.coe").getLines().drop(1).map(_.filter(_.isDigit).toInt).toSeq
    val optimalGraph = RAGn(coe)._3
    val temp = optimalGraph.valueOfOutputs.toIndexedSeq.sorted
    println(optimalGraph.valueOfOutputs.size)
    println(optimalGraph.outputs.mkString(" "))
    randomSim(optimalGraph)
  }
}







