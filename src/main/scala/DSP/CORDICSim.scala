package DSP

import DSP.AlgebricMode._
import DSP.CordicArch._
import DSP.RotationMode._
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, master, slave}

import scala.util.Random

case class cordicTestCase(x: Double, y: Double, z: Double) extends TestCase {
  override def toString: String = s"x: $x, y: $y, z: ${z / Pi * 180.0} degree"
}

case class CordicData() extends Bundle {
  val x = globalType
  val y = globalType
  val z = phaseType()
}

class CORDICSim(rotationMode: RotationMode, algebricMode: AlgebricMode, cordicArch: CordicArch)
  extends Component
    with DSPSim {

  override type inputType = CordicData
  override type outputType = CordicData

  override val input = slave Flow CordicData()
  override val output = master Flow CordicData()

  val config = CordicConfig(algebricMode, rotationMode, cordicArch = cordicArch)
  val cordic = CORDIC(input.payload.x, input.payload.y, input.payload.z, config)
  override val timing: TimingInfo = cordic.getTimingInfo

  output.payload.x := cordic._1.truncated
  output.payload.y := cordic._2.truncated
  output.payload.z := cordic._3.truncated
  output.valid := Delay(input.valid, timing.latency, init = False)

  if (cordicArch == SERIAL) cordic.setStart(input.valid)

  override type TestCase = cordicTestCase
  override type ResultType = Array[Double]

  override def referenceModel(testCase: TestCase) = {

    val x = testCase.x
    val y = testCase.y
    val z = testCase.z
    var xResult = 0.0
    var yResult = 0.0
    var zResult = 0.0
    //    val scaleFactor = 1.0 / CORDIC.getScaleComplement(20)(algebricMode)

    (rotationMode, algebricMode) match {
      case (ROTATION, CIRCULAR) => {
        xResult = x * cos(z) - y * sin(z)
        yResult = y * cos(z) + x * sin(z)
        zResult = 0.0
      }
      case (ROTATION, LINEAR) => {
        xResult = x
        yResult = y + x * z
        zResult = 0.0
      }
      case (ROTATION, HYPERBOLIC) => {
        xResult = x * cosh(z) - y * sinh(z)
        yResult = y * cosh(z) + x * sinh(z)
        zResult = 0.0
      }
      case (VECTORING, CIRCULAR) => {
        xResult = sqrt(x * x + y * y)
        zResult = z + atan(y / x)
        yResult = 0.0
      }
      case (VECTORING, LINEAR) => {
        xResult = x
        zResult = z + y / x
        yResult = 0.0
      }
      case (VECTORING, HYPERBOLIC) => {
        xResult = sqrt(x * x - y * y)
        zResult = z + atanh(y / x)
        yResult = 0.0
      }
    }

    Array(xResult, yResult, zResult)

  }

  override def isValid(refResult: Array[Double], dutResult: Array[Double]): Boolean = sameFixedSeq(refResult, dutResult)

  override def messageWhenInvalid(refResult: Array[Double], dutResult: Array[Double]): String =
    s"[ERROR] \n result: ${dutResult.mkString(" ")} \n golden: ${refResult.mkString(" ")}"

  override def poke(testCase: cordicTestCase, input: CordicData): Unit = {
    input.x.raw #= Double2Fix(testCase.x)
    input.y.raw #= Double2Fix(testCase.y)
    input.z.raw #= Double2Fix(testCase.z)
    clockDomain.waitSampling()
  }

  override def peek(output: CordicData): Array[Double] = {
    val ret = Array(output.x, output.y, output.z).map(Fix2Double(_))
    clockDomain.waitSampling()
    ret
  }
}

//class CORDICSim(cordicConfig: CordicConfig) extends Component with DSPSim{
//  override type inputType = this.type
//  override type outputType = this.type
//  override val input: Flow[CORDICSim.this.type] = _
//  override val output: Flow[CORDICSim.this.type] = _
//  override type TestCase = this.type
//  override type ResultType = this.type
//  override val timing: TimingInfo = _
//
//  override def poke(testCase: CORDICSim.this.type, input: CORDICSim.this.type): Unit = ???
//
//  override def peek(output: CORDICSim.this.type): CORDICSim.this.type = ???
//
//  override def referenceModel(testCase: CORDICSim.this.type): CORDICSim.this.type = ???
//
//  override def isValid(refResult: CORDICSim.this.type, dutResult: CORDICSim.this.type): Boolean = ???
//}

object CORDICSim {
  private val r = Random

  private def randomCase(rotationMode: RotationMode, algebricMode: AlgebricMode) = {
    var x = 0.0
    var y = 0.0
    var z = 0.0

    (rotationMode, algebricMode) match {
      case (VECTORING, CIRCULAR) => {
        val rand = (r.nextDouble() - 0.5) * Pi
        x = cos(rand)
        y = sin(rand)
        z = 0.0
      }
      case (ROTATION, CIRCULAR) => {
        x = 1.0
        y = 0.0
        z = (r.nextDouble() - 0.5) * Pi
      }
      case (VECTORING, HYPERBOLIC) => {
        x = r.nextDouble()
        y = x * r.nextDouble() * 0.8
        z = 0.0
      }
      case (ROTATION, HYPERBOLIC) => {
        x = 1.0
        y = 0.0
        z = (r.nextDouble() - 0.5) * Pi * 0.5
      }
      case (VECTORING, LINEAR) => {
        x = r.nextDouble()
        y = (r.nextDouble() - 0.5) * x
        z = r.nextDouble() - 0.5
      }
      case (ROTATION, LINEAR) => {
        x = r.nextDouble() - 0.5
        y = r.nextDouble() - 0.5
        z = r.nextDouble() - 0.5
      }
    }

    cordicTestCase(x, y, z)
  }

  def randomSim(rotationMode: RotationMode, algebricMode: AlgebricMode, cordicArch: CordicArch): Unit = {

    val dut = SimConfig.withWave.compile(new CORDICSim(rotationMode, algebricMode, cordicArch))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 200) dut.insertTestCase(randomCase(rotationMode, algebricMode))
      val (trueCase, totalCase, log) = dut.simDone()
      println(Console.RED)
      println(log.mkString(" "))
      print(Console.GREEN)
      println(s"[RESULT] ${trueCase} / ${totalCase} passed")
      print(Console.BLACK)
    }
  }

  def main(args: Array[String]): Unit = {

    debug = true

    def doit(rotationMode: RotationMode, algebricMode: AlgebricMode, cordicArch: CordicArch) = {
      randomSim(rotationMode, algebricMode, cordicArch)
      print(Console.GREEN)
      println(s"CORDIC ${rotationMode} + ${algebricMode} + ${cordicArch}, sim done")
      print(Console.BLACK)
    }

    for (arch <- CordicArch.values; algebric <- AlgebricMode.values; rotation <- RotationMode.values) doit(rotation, algebric, arch)

    //    SpinalConfig().generateSystemVerilog(new CORDICSim(ROTATION, CIRCULAR, PARALLEL))
    //    val report = VivadoFlow(new CORDICSim(ROTATION, CIRCULAR, PARALLEL), "CORDIC", "output/CORDIC", force = true).doit()
    //    report.printFMax
    //    report.printArea
  }
}



