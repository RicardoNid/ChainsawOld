package Chainsaw.jpeg

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.language.postfixOps

case class Rgb() extends Bundle {
  val r = UInt(8 bits)
  val g = UInt(8 bits)
  val b = UInt(8 bits)
}

case class YCbCR() extends Bundle {
  val y = UInt(8 bits)
  val cb = UInt(8 bits)
  val cr = UInt(8 bits)
}


//case class JPEGEncoder() {
//
//  val io = new Bundle {
//    val rgbDataIn = in(Rgb())
//    val bitStreamOut = out Bits (32 bits)
//    val validOut = out Bits (3 bits)
//  }
//
//
//}


/**
 * @see ''matlab: rgb2ycbcr''
 */
case class rgb2ycbcr(width: Int) extends Component {

  val io = new Bundle {
    val rgbIn = in(Rgb())
    val ycbcr = out(YCbCR())
  }

  val converterMatrix: Seq[Seq[Double]] = Seq(
    Seq(0.299, 0.587, 0.114),
    Seq(-0.168, -0.331, 0.500),
    Seq(0.500, -0.419, -0.081))

  val converterMatrixAsSFix: Seq[Seq[SFix]] = converterMatrix
    .map(_.map(value => SF(value, 0 exp, -(width - 1) exp)))

  val rgb = Seq(io.rgbIn.r, io.rgbIn.g, io.rgbIn.b)
  val rgbAsSFix: Seq[SFix] = rgb.map(_.intoSInt.toSFix)

  rgbAsSFix.foreach(_.simPublic())
  val products: Seq[Seq[SFix]] = rgbAsSFix.zipWithIndex // Q0.14 * Q8.0 => Q8.15 verilog a * b -> a + b
    .map { case (rgb, i) => converterMatrixAsSFix.map(_ (i)).map(coeff => coeff * rgb) }
  products.foreach(_.foreach(_.simPublic()))

  val rets: Seq[SFix] = (0 until 3).map(i => products.map(_ (i)).reduce(_ +^ _)).map(RegNext(_))

  io.ycbcr.y := rets(0).toSInt.asUInt.resized
  io.ycbcr.cb := rets(1).toSInt.asUInt.resized
  io.ycbcr.cr := rets(2).toSInt.asUInt.resized
}

object rgb2ycbcr extends App {

  val testCase = Array(128, 128, 128).map(_.toDouble)
  val golden: Array[Double] = eng.feval("rgb2ycbcr", testCase).asInstanceOf[Array[Double]]
  println(s"matlab result: ${golden.mkString(" ")}")

  SimConfig.withFstWave.compile(rgb2ycbcr(16)).doSim { dut =>
    dut.clockDomain.forkStimulus(2)
    dut.io.rgbIn.r #= 128
    dut.io.rgbIn.g #= 128
    dut.io.rgbIn.b #= 128

    dut.clockDomain.waitSampling(4)
    println(dut.io.ycbcr.y.toInt)
    println(dut.io.ycbcr.cb.toInt)
    println(dut.io.ycbcr.cr.toInt)
  }

}