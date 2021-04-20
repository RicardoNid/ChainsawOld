package DSP

import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.signal._
import spinal.core._
import spinal.core.sim._

import scala.util.Random

class testCooleyTukeyFFT(length: Int) extends CooleyTukeyFFT(length) with DSPSimOld {
  override type TestCase = Array[Double]
  override type ResultType = DenseVector[Complex]

  override def simInit(): Unit = {
    clockDomain.forkStimulus(2)
    io.input.valid #= false
    clockDomain.waitSampling(10)
  }

  override def simDone(): Unit = {
    clockDomain.waitSampling(10)
    while (refResults.nonEmpty || dutResults.nonEmpty) clockDomain.waitSampling(10)
  }

  override def driver(): Unit = {
    val drv = fork {
      while (true) {
        if (testCases.nonEmpty) {
          val testCase = testCases.dequeue()
          io.input.valid #= true
          (0 until length * 2).foreach(i => io.input.payload(i).raw #= Double2Fix(testCase(i)))
          clockDomain.waitSampling()
          io.input.valid #= false
          val refResult = referenceModel(testCase)
          refResults.enqueue(refResult)
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase) = {
    val complexs = (0 until length).map(i => Complex(testCase(2 * i), testCase(2 * i + 1))).toArray
    fourierTr.dvComplex1DFFT(DenseVector(complexs))
  }

  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (io.output.valid.toBoolean) {
          val dutResult = (0 until length).map { i =>
            val real = Fix2Double(io.output.payload(2 * i))
            val imag = Fix2Double(io.output.payload(2 * i + 1))
            Complex(real, imag)
          }.toArray
          dutResults.enqueue(DenseVector(dutResult))
        }
        clockDomain.waitSampling()
      }
    }
  }

  def same(a: Double, b: Double) = scala.math.abs(a - b) / ((a + b) / 2) < 0.01 || scala.math.abs(a - b) < 0.1

  def sameVector(v1: DenseVector[Complex], v2: DenseVector[Complex]) =
    v1.toArray.zip(v2.toArray).forall { case (c1, c2) => same(c1.real, c2.real) && same(c1.imag, c2.imag) }

  override def isValid(refResult: DenseVector[Complex], dutResult: DenseVector[Complex]): Boolean = sameVector(refResult, dutResult)

  override def scoreBoard(): Unit = {

    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val refResult = refResults.dequeue()
          val dutResult = dutResults.dequeue()
          assert(sameVector(refResult, dutResult), s"$refResult \n $dutResult")
        }
        clockDomain.waitSampling()
      }
    }
  }
}


object testCooleyTukeyFFT {
  private val r = Random

  //    def randomCase(length: Int) = (for (elem <- (0 until 2 * length)) yield (r.nextDouble() * scala.math.pow(2, naturalWidth / 2))).toArray
  private def randomCase(length: Int) = (for (elem <- 0 until 2 * length) yield r.nextDouble()).toArray

  def randomSim(length: Int): Unit = {
    val dut = SimConfig.withWave.compile(new testCooleyTukeyFFT(length))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 100000) dut.insertTestCase(randomCase(length))
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"$length point FFT, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    Array(4, 8, 16).foreach(randomSim)
  }
}
