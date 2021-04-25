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
                        outputWidth: Int = 16, iteration: Int = 11, precision: Int = 15,
                        coarseRotation: Boolean = false, scaleCompensate: Boolean = true)


class CORDIC(inputX: SFix, inputY: SFix, inputZ: SFix, cordicConfig: CordicConfig)
  extends ImplicitArea[(SFix, SFix, SFix)] with DSPDesign with Testable {

  import cordicConfig._

  def magnitudeType(i: Int) = SFix(1 exp, -(14 + i) exp) // TODO: 1QN

  def magnitudeTypeGen(i: Int, value: Double) = SF(value, 1 exp, -(14 + i) exp)

  def phaseType(i: Int) = SFix(2 exp, -(13 + i) exp) // TODO: 2QN

  def phaseTypeGen(i: Int, value: Double) = SF(value, 2 exp, -(13 + i) exp)

  val start = Bool()
  val busy = Bool()

  val outputX = SFix(2 exp, -(outputWidth - 2 - 1) exp)
  val outputY = SFix(2 exp, -(outputWidth - 2 - 1) exp)
  val outputZ = SFix(2 exp, -(outputWidth - 2 - 1) exp)

  // core part
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

      outputX := pipelinedX.truncated
      outputY := pipelinedY.truncated
      outputZ := pipelinedZ.truncated
    }
    case CordicArch.SERIAL => {
      val fsm = new StateMachine {

        val IDLE = new StateDelay(iteration) with EntryPoint
        val WORKING = new StateDelay(iteration)

        val counter = Counter(iteration, isActive(WORKING))
        counter.implicitValue.setName("counter")

        IDLE.whenIsActive(when(start)(goto(WORKING)))
        WORKING.whenCompleted(goto(IDLE))

        busy := isActive(WORKING)

        val signalX = Reg(magnitudeType(iteration))
        val signalY = Reg(magnitudeType(iteration))
        val signalZ = Reg(phaseType(iteration))

        val shiftedX = magnitudeType(iteration)
        val shiftedY = magnitudeType(iteration)
        val phaseCoeff = phaseType(iteration)

        val shiftingCoeffs =
          if (algebricMode == AlgebricMode.HYPERBOLIC) getHyperbolicSequence(iteration)
          else (0 until iteration)
        val shiftingROM = Mem(shiftingCoeffs.map(coeff => U(coeff, log2Up(iteration + 1) bits)))
        val shiftingCoeff = shiftingROM.readAsync(counter)

        // TODO: implement dynamic shifting for fixed type, or this would be very error-prone

        shiftedX.raw := Mux(counter === U(0), inputX.raw << (inputX.minExp - shiftedX.minExp) >> shiftingCoeff, signalX.raw >> shiftingCoeff).resized
        shiftedY.raw := Mux(counter === U(0), inputY.raw << inputY.minExp - shiftedY.minExp >> shiftingCoeff, signalY.raw >> shiftingCoeff).resized

        val phaseROM = Mem((0 until iteration).map(i => phaseTypeGen(iteration, getPhaseCoeff(i)(algebricMode))))
        phaseCoeff := phaseROM.readAsync(counter)
        phaseCoeff.setName("diffZ")

        val counterClockwise = rotationMode match {
          case RotationMode.ROTATION => Mux(counter === U(0), ~inputZ.asBits.msb, ~signalZ.asBits.msb) // Z > 0
          case RotationMode.VECTORING => Mux(counter === U(0), inputY.asBits.msb, signalY.asBits.msb) // Y < 0
        }

        outputX := signalX.truncated
        outputY := signalY.truncated
        outputZ := signalZ.truncated
        WORKING
          .whenIsActive {
            // TODO: implement a better fixed type with clear document
            when(counter === U(0)) {
              val nextX = algebricMode match {
                case DSP.AlgebricMode.CIRCULAR => Mux(counterClockwise, inputX - shiftedY, inputX + shiftedY).truncated
                case DSP.AlgebricMode.HYPERBOLIC => Mux(counterClockwise, inputX + shiftedY, inputX - shiftedY).truncated
                case DSP.AlgebricMode.LINEAR => inputX.truncated
              }
              signalX := nextX
              signalY := Mux(counterClockwise, inputY + shiftedX, inputY - shiftedX).truncated
              signalZ := Mux(counterClockwise, inputZ - phaseCoeff, inputZ + phaseCoeff).truncated
            }.otherwise {
              val nextX = algebricMode match {
                case DSP.AlgebricMode.CIRCULAR => Mux(counterClockwise, signalX - shiftedY, signalX + shiftedY).truncated
                case DSP.AlgebricMode.HYPERBOLIC => Mux(counterClockwise, signalX + shiftedY, signalX - shiftedY).truncated
                case DSP.AlgebricMode.LINEAR => signalX.truncated
              }
              signalX := nextX
              signalY := Mux(counterClockwise, signalY + shiftedX, signalY - shiftedX).truncated
              signalZ := Mux(counterClockwise, signalZ - phaseCoeff, signalZ + phaseCoeff).truncated
            }
          }
      }
    }
  }

  // compensation
  val scaleComplement = magnitudeTypeGen(iteration, getScaleComplement(iteration)(algebricMode))
  val compensatedX = if (scaleCompensate) RegNext(outputX * scaleComplement).truncated else outputX
  val compensatedY = if (scaleCompensate) RegNext(outputY * scaleComplement).truncated else outputY
  val compensatedZ = if (scaleCompensate) RegNext(outputZ) else outputZ

  // TODO: output registration strategy

  override def implicitValue: (SFix, SFix, SFix) = (compensatedX, compensatedY, compensatedZ)

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

  override val getTimingInfo: TimingInfo = {
    val extraDelay = if (scaleCompensate) 1 else 0
    val inputInterval = 1
    val outputInterval = 1
    val latency = cordicArch match {
      case PARALLEL => {
        cordicPipe match {
          case CordicPipe.MAXIMUM => iteration + extraDelay
          case CordicPipe.HALF => iteration / 2 + extraDelay
          case CordicPipe.NONE => extraDelay
        }
      }
      case SERIAL => iteration + extraDelay
    }
    val initiationInterval = cordicArch match {
      case PARALLEL => 1
      case SERIAL => latency
    }
    TimingInfo(inputInterval, outputInterval, latency, initiationInterval)
  }
}

object CORDIC {
  def apply(inputX: SFix, inputY: SFix, inputZ: SFix, cordicConfig: CordicConfig): CORDIC = new CORDIC(inputX, inputY, inputZ, cordicConfig)
}