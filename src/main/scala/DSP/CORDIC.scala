package DSP

import breeze.numerics.constants.Pi
import breeze.numerics._
import spinal.core._

sealed trait RotationMode

object RotationMode {

  case object ROTATION extends RotationMode

  case object VECTORING extends RotationMode

}

sealed trait AlgebricMode

sealed trait CORDICArch

object CIRCULAR extends AlgebricMode

object LINEAR extends AlgebricMode

object HYPERBOLIC extends AlgebricMode


object PIPELINED extends CORDICArch

import DSP.RotationMode._

class CORDIC(mode: RotationMode = ROTATION, arch: CORDICArch = PIPELINED, widthIn: Int = 10, iterations: Int = 20) extends Component {

  val widthOut = widthIn + iterations


}

object CORDIC {
  def cordic(inputX: SFix, inputY: SFix, inputZ: SFix,
             algebricMode: Bits, rotationMode: Bool, iterations: Int, pipelined: Int) = {

    val widthInX = inputX.getBitsWidth
    val widthInY = inputY.getBitsWidth
    val widthIn = if (widthInX > widthInY) widthInX else widthInY
    val widthInZ = inputZ.getBitsWidth
    val widthOut = widthIn + iterations

    def magnitudeDataType(i: Int) = SFix(peak = inputX.maxExp exp, width = (inputX.bitCount + i) bits)

    val circularCoeffcients = Array.ofDim[SInt](iterations)
    (0 until iterations).foreach(i => circularCoeffcients(i) := getPhase(i)(CIRCULAR))

    val circularCoefficientTable = Mem(SInt, circularCoeffcients)

    // TODO: that is amazing!
    val regsX = (0 until iterations).map(i => Reg(magnitudeDataType(i)))
    val regsY = (0 until iterations).map(i => Reg(magnitudeDataType(i)))


  }

  def getPhase(iteration: Int)(implicit algebricMode: AlgebricMode) = {
    algebricMode match {
      case CIRCULAR => atan(pow(2.0, -iteration))
      case LINEAR => -pow(2.0, -iteration)
      case HYPERBOLIC => tanh(pow(2.0, -(iteration + 1)))
    }
  }

  def getScaleComplement(iterations: Int)(implicit algebricMode: AlgebricMode): Double = {
    require(iterations >= 1)

    algebricMode match {
      case CIRCULAR => (0 until iterations).map(i => cos(getPhase(i))).product
      case HYPERBOLIC => (0 until iterations).map(i => sqrt(1 - pow(2.0, -2 * (i + 1)))).product
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val mode = CIRCULAR
    (0 until 10).foreach(i => println(getPhase(i) / Pi * 180.0))
  }
}
