package DSP

import spinal.core._
import spinal.lib._

/** Multiplierless Single Constant Multiplication
 *
 */
class SCM(input: SInt, constant: Int, scmArch: SCMArch) extends ImplicitArea[SInt] with DSPDesign {

  val bitGrowth = log2Up(constant)
  val result = SInt(input.getBitsWidth + bitGrowth bits)

  scmArch match {
    case SCMArch.CSD => {
      val encoded = optimalCSD(constant).reverse
      printlnWhenDebug(s"$constant encoded as: ${encoded} (reverse)")
      val signed = encoded.filterNot(_ == '0').map {
        case '1' => input
        case '9' => -input
      }
      val shifts = encoded.zipWithIndex.filterNot(_._1 == '0').map(_._2)
      printlnWhenDebug(s"shifting values: ${shifts.mkString(" ")}")
      // NUMERIC: the constant itself provides the most accurate prediction
      result := ShiftAdderTree(signed.map(_.toSFix), shifts).toSInt.resized
    }
    case SCMArch.MULT => {
      // NUMERIC: the constant itself provides the most accurate prediction
      result := (input * constant).resized
      result.addAttribute("use_dsp = \"no\"")
    }
  }

  override def implicitValue: SInt = result

  override def getDelay: Int = LatencyAnalysis(input, result)
}

object SCM {
  def apply(input: SInt, constant: Int, scmArch: SCMArch): SCM = new SCM(input, constant, scmArch)
}