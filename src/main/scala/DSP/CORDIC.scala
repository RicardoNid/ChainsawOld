package DSP

import DSP.CORDIC.cordic
import breeze.numerics._
import spinal.core._
import spinal.lib._

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


case class CordicData() extends Bundle {
  val x = data
  val y = data
  val z = data
}

class CORDIC(mode: RotationMode = ROTATION, arch: CORDICArch = PIPELINED, iterations: Int = 20) extends Component {

  val input = slave Flow CordicData()
  val output = master Flow CordicData()

  val outputs = cordic(input.payload.x, input.payload.y, input.payload.z, iterations, CIRCULAR, VECTORING)
  output.payload.x := outputs._1.truncated
  output.payload.y := outputs._2.truncated
  output.payload.z := outputs._3.truncated
  output.valid := Delay(input.valid, iterations, init = False)
  output.valid.init(False)
}

// TODO: algebricMode: Bits, rotationMode: Bool, pipelined: Int
object CORDIC {
  def cordic(inputX: SFix, inputY: SFix, inputZ: SFix,
             iterations: Int, algebricMode: AlgebricMode, rotationMode: RotationMode) = {

    //    val widthInX = inputX.getBitsWidth
    //    val widthInY = inputY.getBitsWidth
    //    val widthIn = if (widthInX > widthInY) widthInX else widthInY
    //    val widthInZ = inputZ.getBitsWidth
    //    val widthOut = widthIn + iterations

    def magnitudeDataType(i: Int) = SFix(peak = inputX.maxExp exp, width = (inputX.bitCount + i) bits)

    def phaseDataType = SFix(peak = inputZ.maxExp exp, width = (inputZ.bitCount) bits)

    val circularCoeffcients = (0 until iterations).map(i => SF(getPhase(i)(CIRCULAR), inputZ.maxExp exp, inputZ.bitCount bits)).toArray
    circularCoeffcients.foreach(_.setName("coeffs"))
    println((0 until iterations).map(i => getPhase(i)(CIRCULAR)).mkString(" "))
    //    val circularCoefficientTable = Mem(SFix, circularCoeffcients)

    val regsX = (0 until iterations).map(i => Reg(magnitudeDataType(16))).toList
    regsX.foreach(_.setName("regsX"))
    val regsY = (0 until iterations).map(i => Reg(magnitudeDataType(16))).toList
    regsY.foreach(_.setName("regsY"))
    val regsZ = (0 until iterations).map(i => Reg(phaseDataType)).toList
    regsZ.foreach(_.setName("regsZ"))

    val shiftedX = (inputX :: regsX.dropRight(1)).zipWithIndex.map { case (regX, i) => regX >> i }
    shiftedX.foreach(_.setName("shiftedX"))
    val shiftedY = (inputY :: regsY.dropRight(1)).zipWithIndex.map { case (regY, i) => regY >> i }
    shiftedY.foreach(_.setName("shiftedY"))

    val counterClockWises =
      rotationMode match {
        case VECTORING => (inputY :: regsY.dropRight(1)).map(_.asBits.msb) // Y < 0
        case ROTATION => regsZ.map(~_.asBits.msb) // Z > 0
      }
    //    counterClockWises.foreach(_.setName("counterClockwise"))

    (inputY :: regsY.dropRight(1)).zip(regsY).zip(counterClockWises.zip(shiftedX))
      .foreach { case ((prev, cur), (counter, shifted)) =>
        cur := Mux(counter, prev + shifted, prev - shifted).truncated
      }

    (inputX :: regsX.dropRight(1)).zip(regsX).zip(counterClockWises.zip(shiftedY))
      .foreach { case ((prev, cur), (counter, shifted)) =>
        algebricMode match {
          case CIRCULAR => cur := Mux(counter, prev - shifted, prev + shifted).truncated
          case LINEAR => cur := prev.truncated
          case HYPERBOLIC => cur := Mux(counter, prev + shifted, prev - shifted).truncated
        }
      }

    (inputZ :: regsZ.dropRight(1)).zip(regsZ).zip(counterClockWises.zip(circularCoeffcients))
      .foreach { case ((prev, cur), (cond, coeff)) =>
        cur := Mux(cond, prev - coeff, prev + coeff).truncated
      }

    val scaleComplement = SF(getScaleComplement(iterations)(CIRCULAR), inputX.maxExp exp, inputX.bitCount bits)
    scaleComplement.setName("scaleComplement")

    (regsX.last * scaleComplement, regsY.last * scaleComplement, regsZ.last)
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
    (0 until 10).foreach(i => println(getPhase(i)))
    (1 until 10).foreach(i => println(getScaleComplement(i)))
    //    VivadoFlow(design = new CORDIC(), topModuleName = "CORDIC", workspacePath = "output/CORIDC", force = true).doit()
  }
}
