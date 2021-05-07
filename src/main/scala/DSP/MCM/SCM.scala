package DSP.MCM

import DSP.{BinaryTreeWithInfo, Coding, SCMArch, Testable, TimingInfo}
import spinal.core._

// TODO: add the implementation by MAG algo
// TODO: implement this by Real and no "resize" at all
/** Single Constant Multiplication by multiplier/CSD
 *
 */
class SCM(input: SInt, constant: Int, scmArch: SCMArch) extends ImplicitArea[SInt] with Testable {

  val result: SInt = scmArch match {
    case SCMArch.CSD =>
      // optimal CSD code, reverse it for a ascending shift value
      val encoded = Coding.optimalCSD(constant).reverse
      val signeds = encoded.filter(_ != '0').map {
        case '1' => input
        case '9' => -input
      }

      val shiftAdd = (left: (SInt, Int), right: (SInt, Int)) => {
        val shiftLeft = right._2 - left._2
        require(shiftLeft >= 0)
        (left._1 +^ (right._1 << shiftLeft), left._2)
      }

      val shifts = encoded.zipWithIndex.filter(_._1 != '0').map(_._2)
      val sat = new BinaryTreeWithInfo(signeds.zip(shifts), shiftAdd)
      sat.implicitValue << sat.getRemainedInfo

    case SCMArch.MULT =>
      input * constant
  }

  override def implicitValue: SInt = scmArch match {
    case SCMArch.CSD => result
    case SCMArch.MULT =>
      result.addAttribute("use_dsp = \"no\"")
      RegNext(result)
  }
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 1, 1)
}

object SCM {
  def apply(input: SInt, constant: Int, scmArch: SCMArch): SCM = new SCM(input, constant, scmArch)
}