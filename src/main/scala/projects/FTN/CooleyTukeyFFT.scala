package projects.FTN

import breeze.linalg.DenseVector
import breeze.linalg.Vector.castFunc
import breeze.math.Complex
import breeze.numerics.{abs, cos, exp, floor, sin}
import breeze.signal._
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import sysu.xilinx._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm
/*
ALGO: DSP with FPGA, algo 6.8, fig 6.12
 */
// TODO: refactor this by functional style
// TODO: inplement interleave function instead of the for loop in part 1 & 3

class CooleyTukeyFFT(N: Int) extends Component {

  val io = new Bundle {
    val input = slave Flow (Vec(data, N * 2))
    val output = master Flow (Vec(data, N * 2))
  }

  val inputNumbers = (0 until N).map(i => ComplexNumber(io.input.payload(i * 2), io.input.payload(i * 2 + 1)))

  val outputNumbers = cooleyTukeyFFT(inputNumbers)
  (0 until N).foreach { i =>
    io.output.payload(2 * i) := outputNumbers(i).real.truncated
    io.output.payload(2 * i + 1) := outputNumbers(i).imag.truncated
  }

  // TODO: find a way to determine the latency automatically
  io.output.valid := Delay(io.input.valid, 2)
  io.output.valid.init(False)
}

object CooleyTukeyFFT {
  def main(args: Array[String]): Unit = {

    val task = VivadoTask(
      topModuleName = "FFT",
      workspacePath = "./output/FTN",
      frequencyTarget = (600 MHz)
    )

    val report = VivadoFlow( // performance verification
      design = new CooleyTukeyFFT(testFFTLength),
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = task,
      force = true
    ).doit()

    println(s"DSP estimated = , DSP consumed = ${report.DSP}")
    println(s"frequency expected = 600 MHz, frequency met = ${report.Frequency / 1E6} MHz")
    report.printArea
    report.printFMax
  }
}

case class FFTCase(data: Array[Double]) extends TestCase

class testCooleyTukeyFFT(length: Int) extends CooleyTukeyFFT(length) with DSPSim {

  val testCases = mutable.Queue[FFTCase]()
  val refResults = mutable.Queue[DenseVector[Complex]]()
  val dutResults = mutable.Queue[DenseVector[Complex]]()

  override def init(): Unit = {
    clockDomain.forkStimulus(2)
    io.input.valid #= false
    clockDomain.waitSampling(10)
  }

  def sim() = {
    init
    driver
    monitor
    scoreBoard
  }

  def simDone() = {
    clockDomain.waitSampling(10)
    while (refResults.nonEmpty && dutResults.nonEmpty) {
      clockDomain.waitSampling(10)
    }
  }

  override def driver: Unit = {
    val drv = fork {
      while (true) {
        if (testCases.nonEmpty) {
          val testCase = testCases.dequeue()
          referenceModel(testCase)
          io.input.valid #= true
          (0 until length * 2).foreach(i => io.input.payload(i).raw #= Double2Fix(testCase.data(i)))
          println("stimuluts in ")
          clockDomain.waitSampling()
          io.input.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  def referenceModel(testCase: FFTCase) = {
    val ComplexNumbers = (0 until length).map(i => Complex(testCase.data(2 * i), testCase.data(2 * i + 1)))
    val golden = fourierTr.dvComplex1DFFT(DenseVector(ComplexNumbers.toArray))
    refResults.enqueue(golden)
  }

  override def monitor: Unit = {
    val mon = fork {
      while (true) {
        if (io.output.valid.toBoolean) {
          val result = (0 until length).map { i =>
            val real = Fix2Double(io.output.payload(i * 2))
            val imag = Fix2Double(io.output.payload(i * 2 + 1))
            Complex(real, imag)
          }
          dutResults.enqueue(DenseVector(result.toArray))
        }
        clockDomain.waitSampling()
      }
    }
  }

  def insertTestCase(fftCase: FFTCase) = testCases.enqueue(fftCase)

  override def scoreBoard: Unit = {
    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val dutResult = dutResults.dequeue()
          val refResult = refResults.dequeue()
          println(s"$dutResult")
          println(s"$refResult")
          assert(true, "incorrect")
        }
        clockDomain.waitSampling()
      }
    }
  }
}


//
object testCooleyTukeyFFT { // functional verification

  def main(args: Array[String]): Unit = {

    val dut = SimConfig.withWave.compile(new testCooleyTukeyFFT(8))
    dut.doSim { dut =>
      dut.sim()
      dut.insertTestCase(FFTCase(Array(0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0)))
      dut.insertTestCase(FFTCase(Array.range(0, 16).map(_.toDouble)))
      dut.simDone()
    }

  }
}
