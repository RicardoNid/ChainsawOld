package DSP

import spinal.core._

// TODO: add the implementation by MAG algo
// TODO: implement this by Real
/** Single Constant Multiplication by multiplier/CSD
 *
 */
class SCM(input: SInt, constant: Int, scmArch: SCMArch) extends ImplicitArea[SInt] with Testable {

  val bitGrowth = log2Up(constant)
  val result = SInt(input.getBitsWidth + bitGrowth bits)

  scmArch match {
    case SCMArch.CSD => {
      val encoded = Coding.optimalCSD(constant).reverse
      printlnWhenDebug(s"$constant encoded as: ${encoded} (reverse)")
      val signeds = encoded.filterNot(_ == '0').map {
        case '1' => input
        case '9' => -input
      }

      val shiftAdd = (left: (SInt, Int), right: (SInt, Int)) => {
        val shiftLeft = right._2 - left._2
        require(shiftLeft >= 0)
        (left._1 +^ (right._1 << shiftLeft), left._2)
      }

      val shifts = encoded.zipWithIndex.filterNot(_._1 == '0').map(_._2)
      printlnWhenDebug(s"shifting values: ${shifts.mkString(" ")}")
      // NUMERIC: the constant itself provides the most accurate prediction
      //      result := ShiftAdderTree(signeds.map(_.toSFix), shifts).toSInt.resized
      val sat = new BinaryTreeWithInfo(signeds.zip(shifts), shiftAdd)
      result := (sat.implicitValue << sat.getRemainedInfo).resized
    }
    case SCMArch.MULT => {
      // NUMERIC: the constant itself provides the most accurate prediction
      result := (input * constant).resized
      result.addAttribute("use_dsp = \"no\"")
    }
  }

  override def implicitValue: SInt = scmArch match {
    case SCMArch.CSD => result
    case SCMArch.MULT => RegNext(result)
  }
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 1, 1)
}

object SCM {
  def apply(input: SInt, constant: Int, scmArch: SCMArch): SCM = new SCM(input, constant, scmArch)
}