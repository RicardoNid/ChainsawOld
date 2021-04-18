package DSP

import breeze.numerics._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

object AlgebricMode extends Enumeration {
  type AlgebricMode = Value
  val CIRCULAR, HYPERBOLIC, LINEAR = Value
}

object RotationMode extends Enumeration {
  type RotationMode = Value
  val ROTATION, VECTORING = Value
}

object CordicArch extends Enumeration {
  type CordicArch = Value
  val PARALLEL, SERIAL = Value
}

object CordicPipe extends Enumeration {
  type CordicPipe = Value
  val MAXIMUM, HALF, NONE = Value
}

import DSP.AlgebricMode.AlgebricMode
import DSP.CordicArch.CordicArch
import DSP.CordicPipe.CordicPipe
import DSP.RotationMode.RotationMode

/** Describe your design here
 *
 * @param algebricMode
 * @param rotationMode
 * @param cordicArch
 * @param cordicPipe
 * @param outputWidth
 * @param iteration
 * @param precision
 * @param coarseRotation
 * @param scaleCompensate
 */

import DSP.CordicArch._
import DSP.CordicPipe._

case class CordicConfig(algebricMode: AlgebricMode, rotationMode: RotationMode,
                        cordicArch: CordicArch = PARALLEL, cordicPipe: CordicPipe = MAXIMUM,
                        outputWidth: Int = 16, iteration: Int = 15, precision: Int = 15,
                        coarseRotation: Boolean = false, scaleCompensate: Boolean = true)


class CORDIC(inputX: SFix, inputY: SFix, inputZ: SFix, cordicConfig: CordicConfig)
  extends ImplicitArea[(SFix, SFix, SFix)] with DSPDesign {

  import cordicConfig._

  def magnitudeType(i: Int) = SFix(2 exp, -13 exp) // TODO: 1QN

  def magnitudeTypeGen(i: Int, value: Double) = SF(value, 2 exp, -13 exp)

  def phaseType(i: Int) = SFix(2 exp, -13 exp) // TODO: 2QN

  def phaseTypeGen(i: Int, value: Double) = SF(value, 2 exp, -13 exp)

  val outputX = SFix(2 exp, -(outputWidth - 2 - 1) exp)
  val outputY = SFix(2 exp, -(outputWidth - 2 - 1) exp)
  val outputZ = SFix(2 exp, -(outputWidth - 2 - 1) exp)

  cordicArch match {
    case CordicArch.PARALLEL => {
      val signalXs = inputX :: (0 until iteration).map(i => magnitudeType(i)).toList
      val signalYs = inputY :: (0 until iteration).map(i => magnitudeType(i)).toList
      val signalZs = inputZ :: (0 until iteration).map(i => phaseType(i)).toList

      val shiftingCoeffs =
        if (algebricMode == AlgebricMode.HYPERBOLIC) getHyperbolicSequence(iteration)
        else (0 until iteration)

      val shiftedXs = signalXs.dropRight(1).zip(shiftingCoeffs).map { case (x, i) => x >> i }
      val shiftedYs = signalYs.dropRight(1).zip(shiftingCoeffs).map { case (y, i) => y >> i }
      val phaseCoeffs = (0 until iteration).map(i => phaseTypeGen(i, getPhaseCoeff(i)(algebricMode)))

      val pipesAll = false :: (0 until iteration).map { i =>
        cordicPipe match {
          case CordicPipe.MAXIMUM => true
          case CordicPipe.HALF => i % 2 == 0
          case CordicPipe.NONE => false
        }
      }.toList
      val pipes = pipesAll.dropRight(1)

      val counterClockwises = rotationMode match {
        case RotationMode.ROTATION => signalZs.dropRight(1).map(~_.asBits.msb) // Z > 0
        case RotationMode.VECTORING => signalYs.dropRight(1).map(_.asBits.msb) // Y < 0
      }

      signalXs.dropRight(1).zip(signalXs.drop(1)) // get the prev and next
        .zip(counterClockwises.zip(pipes)) // get the control signal/conditions
        .zip(shiftedYs) // get the increment
        .foreach { case (((prev, next), (counterClockwise, pipe)), shifted) => // note the format of tuple
          val combX = algebricMode match {
            case AlgebricMode.CIRCULAR => Mux(counterClockwise, prev - shifted, prev + shifted).truncated
            case AlgebricMode.HYPERBOLIC => Mux(counterClockwise, prev + shifted, prev - shifted).truncated
            case AlgebricMode.LINEAR => prev.truncated
          }
          next := (if (pipe) RegNext(combX) else combX).truncated
        }

      signalYs.dropRight(1).zip(signalYs.drop(1)) // get the prev and next
        .zip(counterClockwises.zip(pipes)) // get the control signal/conditions
        .zip(shiftedXs) // get the increment
        .foreach { case (((prev, next), (counterClockwise, pipe)), shifted) => // note the format of tuple
          val combY = Mux(counterClockwise, prev + shifted, prev - shifted).truncated
          next := (if (pipe) RegNext(combY) else combY).truncated
        }

      signalZs.dropRight(1).zip(signalZs.drop(1)) // get the prev and next
        .zip(counterClockwises.zip(pipes)) // get the control signal/conditions
        .zip(phaseCoeffs) // get the increment
        .foreach { case (((prev, next), (counterClockwise, pipe)), coeff) => // note the format of tuple
          val combZ = Mux(counterClockwise, prev - coeff, prev + coeff).truncated
          next := (if (pipe) RegNext(combZ) else combZ).truncated
        }

      val pipelinedX = if (pipesAll.last) RegNext(signalXs.last) else signalXs.last
      val pipelinedY = if (pipesAll.last) RegNext(signalYs.last) else signalYs.last
      val pipelinedZ = if (pipesAll.last) RegNext(signalZs.last) else signalZs.last

      val scaleComplement = magnitudeTypeGen(iteration, getScaleComplement(iteration)(algebricMode))
      val compensatedX = (if (scaleCompensate) RegNext(pipelinedX * scaleComplement).truncated else pipelinedX)
      val compensatedY = (if (scaleCompensate) RegNext(pipelinedY * scaleComplement).truncated else pipelinedY)
      val compensatedZ = if (scaleCompensate) RegNext(pipelinedZ) else pipelinedZ

      outputX := compensatedX.truncated
      outputY := compensatedY.truncated
      outputZ := compensatedZ.truncated
    }
    case CordicArch.SERIAL => {
      val fsm = new StateMachine {
        val working = new StateDelay(iteration) with EntryPoint

        val counter = Counter(iteration)
        val signalX = Reg(magnitudeType(0))
        val signalY = Reg(magnitudeType(0))
        val signalZ = Reg(phaseType(0))

        val shiftedX = magnitudeType(0)
        val shiftedY = magnitudeType(0)

        val phaseROM = Mem((0 until iteration).map(i => phaseTypeGen(i, getPhaseCoeff(i)(algebricMode))))

        val counterClockwise = rotationMode match {
          case RotationMode.ROTATION => ~signalZ.asBits.msb // Z > 0
          case RotationMode.VECTORING => signalY.asBits.msb // Y < 0
        }

        working
          .whenCompleted(goto(working))
          .whenIsActive {
            counter.increment()
            val phaseCoeff = phaseROM.readSync(counter)
            // TODO: implement a better fixed type with clear document
            shiftedX := signalX.raw >> counter.value
            shiftedY := signalY.raw >> counter.value
            when(counter === U(0)) {


            }.otherwise {
              signalX := algebricMode match {
                case DSP.AlgebricMode.CIRCULAR => Mux(counterClockwise, signalX - shiftedY, signalX + shiftedY).truncated
                case DSP.AlgebricMode.HYPERBOLIC => Mux(counterClockwise, signalX + shiftedY, signalX - shiftedY).truncated
                case DSP.AlgebricMode.LINEAR => signalX.truncated
              }
              signalY := Mux(counterClockwise, signalY + shiftedX, signalY - shiftedX).truncated
              signalZ := Mux(counterClockwise, signalZ - phaseCoeff, signalZ + phaseCoeff).truncated
            }
          }
      }
    }
  }


  override def implicitValue: (SFix, SFix, SFix) = (outputX, outputY, outputZ)

  val extraDelay = if (scaleCompensate) 1 else 0

  override def getDelay: Int = cordicPipe match {
    case CordicPipe.MAXIMUM => iteration + extraDelay
    case CordicPipe.HALF => iteration / 2 + extraDelay
    case CordicPipe.NONE => extraDelay
  }


  def getHyperbolicSequence(iteration: Int) = {
    require(iteration < 54, "iteration times should be less than 54")
    val sequence = (1 to 4) ++ (4 to 13) ++ (13 to 40) ++ (40 to 50)
    sequence.slice(0, iteration)
  }

  /** Get the phase coefficient stored in LUT for Z iteration
   *
   * @param iteration current iteration
   * @param algebricMode
   * @return
   */
  def getPhaseCoeff(iteration: Int)(implicit algebricMode: AlgebricMode) = {
    algebricMode match {
      case AlgebricMode.CIRCULAR => atan(pow(2.0, -iteration))
      case AlgebricMode.HYPERBOLIC => atanh(pow(2.0, -getHyperbolicSequence(iteration + 1).last))
      case AlgebricMode.LINEAR => pow(2.0, -iteration)
    }
  }

  def getScaleComplement(iteration: Int)(implicit algebricMode: AlgebricMode) = {
    require(iteration >= 1)
    algebricMode match {
      case AlgebricMode.CIRCULAR => (0 until iteration).map(i => cos(getPhaseCoeff(i))).product
      case AlgebricMode.HYPERBOLIC => 1.0 / (1 until iteration)
        .map(i => getHyperbolicSequence(i).last)
        .map(i => sqrt(1 - pow(2.0, -2 * i))).product
      case AlgebricMode.LINEAR => 1.0
    }
  }
}

object CORDIC {
  def apply(inputX: SFix, inputY: SFix, inputZ: SFix, cordicConfig: CordicConfig): CORDIC = new CORDIC(inputX, inputY, inputZ, cordicConfig)
}