package Chainsaw.MCM

import Chainsaw.Architectures.{BinarySFG, BinaryTreeWithInfo, HomogeneousBinarySFGBuilder}
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
class SCM(input: Real, constant: Int, scmArch: SCMArch) extends ImplicitArea[Real] with Testable {

  val result: Real = scmArch match {

    case SCMArch.MAG =>
      printlnYellow(BinarySFG.fromSerialized(MAG.getOnePathLUT(constant)._2))
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

      val shiftAdd = (left: (Real, Int), right: (Real, Int)) => {
        val shiftLeft = right._2 - left._2
        require(shiftLeft >= 0)
        printlnWhenDebug(right._1.realInfo)
        printlnWhenDebug(left._1.realInfo)
        val shifted = right._1 << shiftLeft
        printlnWhenDebug(shifted.realInfo)
        val ret = left._1 + shifted
        printlnWhenDebug(ret.realInfo)
        (ret, left._2)
      }

      val shifts = encoded.zipWithIndex.filter(_._1 != '0').map(_._2)

      val sat = new BinaryTreeWithInfo(signeds.zip(shifts), shiftAdd)

      sat.implicitValue << sat.getRemainedInfo

    case SCMArch.MULT =>
      input * constant
  }

  printlnGreen(s"SCM result: ${result.realInfo}")

  override def implicitValue: Real = scmArch match {
    case SCMArch.MAG => RegNext(result)
    case SCMArch.CSD => result
    case SCMArch.MULT =>
      result.addAttribute("use_dsp = \"no\"")
      RegNext(result)
  }
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 1, 1)
}

object SCM {
  def apply(input: Real, constant: Int, scmArch: SCMArch): SCM = new SCM(input, constant, scmArch)

  def main(args: Array[String]): Unit = {
    println(MAG.getOnePathLUT(12345))
  }
}