package FTN

import Chainsaw._
import com.mathworks.engine.MatlabEngine
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import matlabIO._

import scala.collection.mutable.ListBuffer

/** Config of conv encoding
 *
 * @param length            constraint length
 * @param gens              generator polynomial, in octal form
 * @param codeRate          in the form n/m
 * @param puncturedCodeRate in the form n/m
 */
// TODO: implement this for multiple streams
// TODO: implement punctuations and arbitrary codeRate
case class ConvencConfig(length: Int, gens: Array[Int])

class Convenc(input: Bool, config: ConvencConfig) extends ImplicitArea[Vec[Bool]] with Testable {

  def octal2BinaryString(gen: Int) =
    gen.toString.flatMap(_.asDigit.toBinaryString.reverse.padTo(3, '0').reverse)

  val genCount = config.gens.length

  val trellises = config.gens.map(octal2BinaryString).map(_.takeRight(config.length))
  //  val trellises = config.gens.map(octal2BinaryString).map(_.reverse.take(config.length))
  println("trellises: \n" + trellises.mkString("\n"))
  //  val regs = Vec(RegInit(False), config.length - 1)
  val regs = Vec(RegInit(False), config.length)
  //  val srl = input +: regs
  regs.head := input
  //  srl.init.zip(srl.tail).foreach { case (prev, next) => next := prev }
  regs.init.zip(regs.tail).foreach { case (prev, next) => next := prev }

  //  val rets = trellises.map(trellis => trellis.zip(srl).filter(_._1 == '1').map(_._2).xorR)
  val rets = trellises.map(trellis => trellis.zip(regs).filter(_._1 == '1').map(_._2).xorR)

  override def implicitValue: Vec[Bool] = Vec(rets)

  def referenceModel(bits: Array[Boolean]) = {
    //    val eng = MatlabEngine.startMatlab
    eng.putVariable("bits", bits)
    eng.eval(s"trellis = poly2trellis(${config.length}, ${config.gens.asMatlab})")
    eng.eval(s"convenc(bits, trellis)")
    val ret = eng.getVariable("ans").asInstanceOf[Array[Boolean]]
    //    eng.close()
    ret
  }

  override val getTimingInfo: TimingInfo = TimingInfo(11, 11, 1, 18)
}

class ConvencDUT(config: ConvencConfig) extends DSPDUTTiming[Bool, Vec[Bool]] {
  override val input: Bool = in Bool
  val convenc = new Convenc(input, config)
  override val output: Vec[Bool] = out(convenc.implicitValue)
  override val timing: TimingInfo = convenc.getTimingInfo
}