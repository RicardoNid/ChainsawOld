//package Chainsaw
//
//import Chainsaw.FIRArch._
//import breeze.linalg.DenseVector
//import breeze.signal._
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//
//import scala.collection.mutable.ListBuffer
//import scala.language.postfixOps
//import scala.util.Random
//
//class FIRDUT(coefficients: IndexedSeq[Double], firArch: FIRArch, inputMaxExp: Int = naturalWidth, inputMinExp: Int = -fractionalWidth) extends DSPGen {
//
//  require(inputMaxExp >= 0 && inputMinExp <= 0)
//
//  private def localType = SFix(inputMaxExp exp, inputMinExp exp)
//
//  val input = slave Flow localType
//  printlnWhenDebug(s"input range: ${input.payload.maxValue}, ${input.payload.minValue}")
//  val ZERO = SF(0.0, localType.maxExp exp, localType.minExp exp)
//  val actualInput = Mux(input.valid, input.payload, ZERO)
//
//  val body = FIR(actualInput, coefficients, firArch)
//
//  val output = master Flow cloneOf(body.implicitValue)
//  output.payload := body.implicitValue
//
//  output.valid := Delay(input.valid, body.delay, init = False)
//  output.valid.init(False)
//
//  override def delay: Int = body.delay
//}
//
//class FIRSim(coefficients: Array[Double], FIRArch: FIRArch, inputMaxExp: Int = naturalWidth, inputMinExp: Int = -fractionalWidth)
//  extends FIRDUT(coefficients, FIRArch, inputMaxExp, inputMinExp) with DSPSimOld {
//
//  override type TestCase = Array[Double]
//  override type ResultType = DenseVector[Double]
//  override val exitPeriod: Int = coefficients.length
//
//  override def simInit(): Unit = {
//    input.valid #= false
//    super.simInit()
//  }
//
//  override def isValid(refResult: DenseVector[Double], dutResult: DenseVector[Double]): Boolean = sameFixedVector(refResult, dutResult)
//
//  override def referenceModel(testCase: TestCase) = filter(DenseVector(testCase), DenseVector(coefficients))
//
//  override def driver(): Unit = {
//    fork {
//      while (true) {
//        if (testCases.nonEmpty) {
//          val testCase = testCases.dequeue()
//          //          printlnWhenDebug(s"time: $simTime testCase = ${testCase.mkString(" ")}")
//          val refResult = referenceModel(testCase)
//          //          printlnWhenDebug(s"time: $simTime refResult = $refResult")
//          refResults.enqueue(refResult)
//          for (data <- testCase) {
//            input.valid #= true
//            input.payload.raw #= Double2Fix(data, -inputMinExp)
//            clockDomain.waitSampling()
//          }
//          input.valid #= false
//          clockDomain.waitSampling(coefficients.length + 1)
//        }
//        else clockDomain.waitSampling()
//      }
//    }
//
//  }
//
//  override def monitor(): Unit = {
//    fork {
//      val outputBuffer = ListBuffer[Double]()
//      while (true) {
//        if (output.valid.toBoolean) {
//          outputBuffer += Fix2Double(output.payload, -inputMinExp)
//        }
//        else {
//          if (outputBuffer.nonEmpty) {
//            val dutResult = DenseVector(outputBuffer.toArray)
//            dutResults.enqueue(dutResult)
//            //            printlnWhenDebug(s"time: $simTime dutResult = $dutResult")
//            outputBuffer.clear()
//          }
//        }
//        clockDomain.waitSampling()
//      }
//    }
//  }
//}
//
//object FIRSim {
//  private val r = Random
//
//  private def randomCase(min: Int, maxExp: Int) = {
//    val length = r.nextInt(10) + min
//    (0 until length).map(_ => randData(maxExp)).toArray
//    //            Array(1.0,2.0,3.0,4.0,5.0,6.0,7.0)
//  }
//
//  private def randomCoeff(length: Int, maxExp: Int) = {
//    (0 until length).map(_ => randData(maxExp)).toArray
//    //    Array(1.0, 2.0, 3.0, 4.0, 5.0)
//  }
//
//  def randomSim(coeffLength: Int, arch: FIRArch, inputMaxExp: Int = naturalWidth, inputMinExp: Int = -fractionalWidth): Unit = {
//    val coeff = randomCoeff(coeffLength, inputMaxExp)
//    printlnWhenDebug(s"current coefficients: ${coeff.mkString(" ")}")
//    val dut = SimConfig.withWave.compile(new FIRSim(coeff, arch, inputMaxExp, inputMinExp))
//    dut.doSim { dut =>
//      dut.sim()
//      printlnWhenDebug(s"example testCase: ${randomCase(coeffLength, inputMaxExp).mkString(" ")}")
//      for (_ <- 0 until 100) dut.insertTestCase(randomCase(coeffLength, inputMaxExp))
//      dut.simDone()
//    }
//    print(Console.GREEN)
//    println(s"$coeffLength tap FIR, PASS")
//    print(Console.BLACK)
//  }
//
//  def main(args: Array[String]): Unit = {
//    debug = true
//    //    (5 until 20).foreach(i => randomSim(i, MAC))
//    (5 until 20).foreach(i => randomSim(i, DA, naturalWidth / 2, -(fractionalWidth / 2)))
//  }
//}
//
