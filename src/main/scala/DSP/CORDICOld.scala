//package DSP
//
//import breeze.numerics._
//import spinal.core._
//
//import scala.collection.mutable.ListBuffer
//
//sealed trait RotationMode
//
//object RotationMode {
//
//  case object ROTATION extends RotationMode
//
//  case object VECTORING extends RotationMode
//
//}
//
//sealed trait AlgebricMode
//
//object AlgebricMode {
//
//  case object CIRCULAR extends AlgebricMode
//
//  case object LINEAR extends AlgebricMode
//
//  case object HYPERBOLIC extends AlgebricMode
//
//}
//
//sealed trait CORDICArch
//
//object CORDICArch {
//
//  case object PIPELINED extends CORDICArch
//
//  case object ITERATIVE extends CORDICArch
//
//}
//
//import DSP.AlgebricMode._
//import DSP.RotationMode._
//
//case class CordicData() extends Bundle {
//  val x = globalType
//  val y = globalType
//  val z = phaseType()
//}
//
//// TODO: algebricMode: Bits, rotationMode: Bool, pipelined: Int
//class CORDIC(inputX: SFix, inputY: SFix, inputZ: SFix,
//             rotationMode: RotationMode, algebricMode: AlgebricMode,
//             iterations: Int) extends ImplicitArea[(SFix, SFix, SFix)] with DSPDesign {
//
//
//  def magnitudeDataType(i: Int) = SFix(peak = inputX.maxExp exp, width = (inputX.bitCount + i) bits)
//
//  def phaseDataType = SFix(peak = inputZ.maxExp exp, width = (inputZ.bitCount) bits)
//
//  // coefficients and complements
//  val phaseCoeffcients = (0 until iterations).map(i => SF(getPhase(i)(algebricMode), inputZ.maxExp exp, inputZ.bitCount bits)).toArray
//  val shiftingCoeffs = if (algebricMode == HYPERBOLIC) getHyperbolicSequence(iterations) else (0 until iterations)
//  phaseCoeffcients.foreach(_.setName("coeffs"))
//  val scaleComplement = MySF(getScaleComplement(iterations)(algebricMode), 0.001)
//  scaleComplement.setName("scaleComplement")
//  //    println("phase: " + (0 until iterations).map(i => getPhase(i)(algebricMode)).mkString(" "))
//  //    println("shift: " + shiftingCoeffs.mkString(" "))
//  //    println("scaleComplement: " + getScaleComplement(iterations)(algebricMode).toString)
//
//  //
//  val regsX = (0 until iterations).map(i => Reg(magnitudeDataType(16))).toList
//  regsX.foreach(_.setName("regsX"))
//  val regsY = (0 until iterations).map(i => Reg(magnitudeDataType(16))).toList
//  regsY.foreach(_.setName("regsY"))
//  val regsZ = (0 until iterations).map(i => Reg(phaseDataType)).toList
//  regsZ.foreach(_.setName("regsZ"))
//
//  val shiftedX = (inputX :: regsX.dropRight(1)).zip(shiftingCoeffs).map { case (regX, i) => regX >> i }
//  shiftedX.foreach(_.setName("shiftedX"))
//  val shiftedY = (inputY :: regsY.dropRight(1)).zip(shiftingCoeffs).map { case (regY, i) => regY >> i }
//  shiftedY.foreach(_.setName("shiftedY"))
//
//  val counterClockWises =
//    rotationMode match {
//      case VECTORING => (inputY :: regsY.dropRight(1)).map(_.asBits.msb) // Y < 0
//      case ROTATION => (inputZ :: regsZ.dropRight(1)).map(~_.asBits.msb) // Z > 0
//    }
//  //    counterClockWises.foreach(_.setName("counterClockwise"))
//
//  (inputY :: regsY.dropRight(1)).zip(regsY).zip(counterClockWises.zip(shiftedX))
//    .foreach { case ((prev, cur), (counter, shifted)) =>
//      cur := Mux(counter, prev + shifted, prev - shifted).truncated
//    }
//
//  (inputX :: regsX.dropRight(1)).zip(regsX).zip(counterClockWises.zip(shiftedY))
//    .foreach { case ((prev, cur), (counter, shifted)) =>
//      algebricMode match {
//        case CIRCULAR => cur := Mux(counter, prev - shifted, prev + shifted).truncated
//        case LINEAR => cur := prev.truncated
//        case HYPERBOLIC => cur := Mux(counter, prev + shifted, prev - shifted).truncated
//      }
//    }
//
//  (inputZ :: regsZ.dropRight(1)).zip(regsZ).zip(counterClockWises.zip(phaseCoeffcients))
//    .foreach { case ((prev, cur), (cond, coeff)) =>
//      cur := Mux(cond, prev - coeff, prev + coeff).truncated
//    }
//
//  def getHyperbolicSequence(iterations: Int) = { // length = iterations
//
//    require(iterations >= 1)
//
//    var currentSpecial = 1
//    var i = iterations - 1
//    val result = ListBuffer(1)
//    while (i > 0) {
//      if (result.last == (currentSpecial * 3 + 1)) {
//        result += result.last
//        currentSpecial = currentSpecial * 3 + 1
//      }
//      else result += (result.last + 1)
//      i -= 1
//    }
//    result.toIndexedSeq
//  }
//
//  def getPhase(iteration: Int)(implicit algebricMode: AlgebricMode) = {
//    algebricMode match {
//      case CIRCULAR => atan(pow(2.0, -iteration))
//      case LINEAR => pow(2.0, -iteration)
//      case HYPERBOLIC => atanh(pow(2.0, -getHyperbolicSequence(iteration + 1).last))
//    }
//  }
//
//  def getScaleComplement(iterations: Int)(implicit algebricMode: AlgebricMode): Double = {
//    require(iterations >= 1)
//
//    algebricMode match {
//      case CIRCULAR => (0 until iterations).map(i => cos(getPhase(i))).product
//      case LINEAR => 1.0
//      case HYPERBOLIC => 1.0 / (1 to iterations)
//        .map(i => getHyperbolicSequence(i).last)
//        .map(i => sqrt(1 - pow(2.0, -2 * i))).product
//    }
//  }
//
//  // TODO: implement this with SCM
//  override def implicitValue: (SFix, SFix, SFix) = (regsX.last * scaleComplement.truncated, regsY.last * scaleComplement, regsZ.last)
//
//  override def getDelay: Int = iterations
//}
//
//object CORDIC {
//
//  def apply(inputX: SFix, inputY: SFix, inputZ: SFix, rotationMode: RotationMode, algebricMode: AlgebricMode, iterations: Int): CORDIC = new CORDIC(inputX, inputY, inputZ, rotationMode, algebricMode, iterations)
//
//  def sin(phase: SFix) = {
//    val x, y = globalType
//    x := 1.0
//    y := 0.0
//    new CORDIC(x, y, phase, ROTATION, CIRCULAR, 20)._2
//  }
//
//  def main(args: Array[String]): Unit = {
//
//  }
//}