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
    val eng = MatlabEngine.startMatlab
    eng.putVariable("bits", bits)
    eng.eval(s"trellis = poly2trellis(${config.length}, ${config.gens.asMatlab})")
    eng.eval(s"convenc(bits, trellis)")
    val ret = eng.getVariable("ans").asInstanceOf[Array[Boolean]]
    eng.close()
    ret
  }

  override val getTimingInfo: TimingInfo = TimingInfo(11, 11, 1, 18)
}

class ConvencDUT(config: ConvencConfig) extends  DSPDUTTiming[Bool, Vec[Bool]] {
  override val input: Bool = in Bool
  val convenc = new Convenc(input, config)
  override val output: Vec[Bool] = out(convenc.implicitValue)
  override val timing: TimingInfo = convenc.getTimingInfo
}



class ConvencSim(config: ConvencConfig) extends ConvencDUT(config) with DSPSimTiming[Bool, Vec[Bool], Array[Boolean], Array[Boolean]] {

  override def poke(testCase: Array[Boolean], input: Bool): Unit = {
    testCase.init.foreach { bit =>
      input #= bit
      clockDomain.waitSampling()
    }
    input #= testCase.last
  }

  override def peek(output: Vec[Bool]): Array[Boolean] = {
    val buffer = ListBuffer[Boolean]()
    (0 until timing.outputInterval).init.foreach { _ =>
      val temp = output.map(_.toBoolean)
      buffer ++= output.map(_.toBoolean)
      clockDomain.waitSampling()
    }
    buffer ++= output.map(_.toBoolean)
    buffer.toArray
  }

  override def referenceModel(testCase: Array[Boolean]): Array[Boolean] = convenc.referenceModel(testCase)

  override def isValid(refResult: Array[Boolean], dutResult: Array[Boolean]): Boolean = refResult.zip(dutResult).forall(pair => pair._1 == pair._2)

  override def messageWhenInvalid(testCase: Array[Boolean], refResult: Array[Boolean], dutResult: Array[Boolean]): String = {
    val rowCount = config.gens.length
    val refOutput = (0 until rowCount).map(i => refResult.zipWithIndex.filter(_._2 % rowCount == i).map(_._1).map(if (_) 1 else 0).mkString(" ")).mkString("\n")
    val dutOutput = (0 until rowCount).map(i => dutResult.zipWithIndex.filter(_._2 % rowCount == i).map(_._1).map(if (_) 1 else 0).mkString(" ")).mkString("\n")
    s"testCase: ${testCase.map(if (_) 1 else 0).mkString(" ")}\ngolden:\n${refOutput}\nyours:\n${dutOutput}\n"
  }

  override def messageWhenValid(testCase: Array[Boolean], refResult: Array[Boolean], dutResult: Array[Boolean]): String = {
    messageWhenInvalid(testCase, refResult, dutResult)
  }
}

object Convenc {

  def main(args: Array[String]): Unit = {

    ChainsawDebug = true
    SimConfig.withWave.compile(new ConvencSim(ConvencConfig(7, Array(145, 133)))).doSim { dut =>
      dut.sim()
      dut.insertTestCase(Array(false, false, false, false, false, true, true, true, true, true) :+ false)
      dut.insertTestCase(Array.fill(10)(false) :+ false)
      dut.insertTestCase(Array(true, false, true, false, true, false, true, false, true, false) :+ false)
      val report = dut.simDone()
    }
  }
}
