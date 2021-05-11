package Chainsaw.MCM

import Chainsaw._
import spinal.core._

// TODO: add the implementation by MAG algo
// TODO: implement this by Real and no "resize" at all

object SCMArch extends Enumeration {
  type SCMArch = Value
  val CSD, MAG, MULT = Value
}

import Chainsaw.MCM.AOperations._
import Chainsaw.MCM.SCMArch._

/** Single Constant Multiplication by multiplier/CSD
 *
 */
class SCM(input: SReal, constant: Int, scmArch: SCMArch) extends ImplicitArea[SReal] with Testable {

  val result: SReal = scmArch match {

    case SCMArch.MAG =>
      val (mag, magInfos) = MAG(constant)
      val graph = new HomogeneousBinarySFGBuilder(Seq(input), mag, AOpHardware, magInfos)
      graph.implicitValue.head

    case SCMArch.CSD =>
      // optimal CSD code, reverse it for a ascending shift value
      val encoded = Coding.optimalCSD(constant).reverse
      val signeds = encoded.filter(_ != '0').map {
        case '1' => input
        case '9' => -input
      }

      val shiftAdd = (left: (SReal, Int), right: (SReal, Int)) => {
        val shiftLeft = right._2 - left._2
        require(shiftLeft >= 0)
        println(right._1.numericInfo)
        println(left._1.numericInfo)
        println((right._1 << shiftLeft).numericInfo)
        println((left._1 + (right._1 << shiftLeft)).numericInfo)
        (left._1 + (right._1 << shiftLeft), left._2)
      }

      val shifts = encoded.zipWithIndex.filter(_._1 != '0').map(_._2)

      val sat = new BinaryTreeWithInfo(signeds.zip(shifts), shiftAdd)

      sat.implicitValue << sat.getRemainedInfo

    //    case SCMArch.MULT =>
    //      input * constant
  }

  println(s"result: ${result.numericInfo}")

  override def implicitValue: SReal = scmArch match {
    case SCMArch.MAG => RegNext(result)
    case SCMArch.CSD => result
    //    case SCMArch.MULT =>
    //      result.addAttribute("use_dsp = \"no\"")
    //      RegNext(result)
  }
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 1, 1)
}

object SCM {
  def apply(input: SReal, constant: Int, scmArch: SCMArch): SCM = new SCM(input, constant, scmArch)
}