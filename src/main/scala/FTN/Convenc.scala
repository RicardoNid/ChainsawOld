package FTN

import Chainsaw._
import matlabIO._
import spinal.core._
import spinal.lib._

/** Config of conv encoding
 *
 * @param constLen          constraint length
 * @param codeGens          generator polynomial, in octal form
 * @param codeRate          in the form n/m
 * @param puncturedCodeRate in the form n/m
 */
// TODO: implement this for multiple streams
// TODO: implement punctuations and arbitrary codeRate

class Convenc(input: Bool, config: ConvencConfig) extends ImplicitArea[Vec[Bool]] with Testable {

  def octal2BinaryString(gen: Int) =
    gen.toString.flatMap(_.asDigit.toBinaryString.reverse.padTo(3, '0').reverse)

  val genCount = config.codeGens.length

  val trellises = config.codeGens.map(octal2BinaryString).map(_.takeRight(config.K))
  printlnWhenDebug("trellises: \n" + trellises.mkString("\n"))
  val regs = Vec(RegInit(False), config.K)
  regs.head := input
  regs.init.zip(regs.tail).foreach { case (prev, next) => next := prev }

  //  val rets = trellises.map(trellis => trellis.zip(srl).filter(_._1 == '1').map(_._2).xorR)
  val rets = trellises.map(trellis => trellis.zip(regs).filter(_._1 == '1').map(_._2).xorR)

  override def implicitValue: Vec[Bool] = Vec(rets)

  //  def referenceModel(bits: Array[Boolean]) = {
  //    eng.putVariable("bits", bits)
  //    eng.eval(s"trellis = poly2trellis(${config.length}, ${config.gens.asMatlab});")
  //    eng.eval(s"convenc(bits, trellis);")
  //    val ret = eng.getVariable("ans").asInstanceOf[Array[Boolean]]
  //    ret
  //  }

  def referenceModel(bits: Array[Double]) = MatlabRef.convenc(bits, config)

  override val getTimingInfo: TimingInfo = TimingInfo(7168, 7168, 1, 7200)
}

object Convenc {
  def apply(input: Bool, config: ConvencConfig): Convenc = new Convenc(input, config)
}

class ConvencDUT(config: ConvencConfig) extends DSPDUTTiming[Bool, Vec[Bool]] {
  override val input: Bool = in Bool
  val convenc = new Convenc(input, config)
  override val output: Vec[Bool] = out(convenc.implicitValue)
  override val timing: TimingInfo = convenc.getTimingInfo
}