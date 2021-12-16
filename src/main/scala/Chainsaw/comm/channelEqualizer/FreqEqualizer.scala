package Chainsaw.comm.channelEqualizer

import Chainsaw._
import breeze.numerics.abs
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class VecAdd(dspType: HardType[SFix], vecSize: Int) extends Component {
  val xs, ys = in(Vec(dspType(), vecSize))
  val rets = out(Vec(dspType(), vecSize))

  rets.zip(xs.zip(ys)).foreach { case (ret, (x, y)) => ret := x + y }

  def drive(xOuter: Vec[SFix], yOuter: Vec[SFix], retOuter: Vec[SFix]) = {
    Seq(xOuter, yOuter).zip(Seq( xs, ys)).foreach { case (signal, port) =>
      if (signal == null) port.assignDontCare() else port := signal
    }
    if (retOuter != null) retOuter := rets
  }

  val add: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = drive( _, _, _)

  def idle() = drive(null, null, null)

  def init() = {
    Seq(xs, ys).foreach(_.allowOverride)
    idle()
  }
}

case class VecAddSub(dspType: HardType[SFix], vecSize: Int) extends Component {
  val xs, ys = in(Vec(dspType(), vecSize))
  val mode = in Bits (1 bits)
  //  mode.addAttribute("max_fanout", 32)
  val rets = out(Vec(dspType(), vecSize))

  rets.zip(xs.zip(ys)).foreach { case (ret, (x, y)) => ret := Mux(mode(0), x + y, x - y) }

  def drive(modeOuter: Bits, xOuter: Vec[SFix], yOuter: Vec[SFix], retOuter: Vec[SFix]) = {
    Seq(modeOuter, xOuter, yOuter).zip(Seq(mode, xs, ys)).foreach { case (signal, port) =>
      if (signal == null) port.assignDontCare() else port := signal
    }
    if (retOuter != null) retOuter := rets
  }

  val add: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = drive(B"1", _, _, _)
  val sub: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = drive(B"0", _, _, _)

  def idle() = drive(null, null, null, null)

  def init() = {
    Seq(mode, xs, ys).foreach(_.allowOverride)
    idle()
  }
}

case class VecMult(dspType: HardType[SFix], vecSize: Int) extends Component {
  val xs, ys = in(Vec(dspType(), vecSize))
  val rets = out(Vec(dspType(), vecSize))

  rets.zip(xs.zip(ys)).foreach { case (ret, (x, y)) =>
    val trunc = dspType()
    trunc := (x * y).truncated
    ret := RegNext(trunc)
  }

  def drive(xOuter: Vec[SFix], yOuter: Vec[SFix], retOuter: Vec[SFix]) = {
    Seq(xOuter, yOuter).zip(Seq(xs, ys)).foreach { case (signal, port) =>
      if (signal == null) port.assignDontCare() else port := signal
    }
    if (retOuter != null) retOuter := rets
  }

  val mult: (Vec[SFix], Vec[SFix]) => Unit = drive(_, _, null)

  def idle() = drive(null, null, null)

  def init() = {
    Seq(xs, ys).foreach(_.allowOverride)
    idle()
  }

}



/** channel equalizer, mapping to Xilinx DSP slices
 *
 * @param golden    golden preamble sequence
 * @param iteration number of cycles we used for division
 */
case class FreqEqualizer(golden: Seq[Int], iteration: Int) extends Component {


}

object SynthBig extends App {
  VivadoSynth(Equalization(HardType(SFix(7 exp, 18 bits)), 256), "EqualBig")
}

object SynthSmall extends App {
  VivadoSynth(Equalization(HardType(SFix(7 exp, 18 bits)), 1), "EqualSmall")
}

object EqualizationTest {
  def main(args: Array[String]): Unit = {

    SimConfig.withWave.compile(Equalization(SFix(7 exp, 18 bits), 1)).doSim { dut =>
      val dutResult = ArrayBuffer[Seq[BComplex]]()
      val dataIn = ChainsawRand.nextComplex()
      val preamble = ChainsawRand.nextComplex()
      dut.clockDomain.forkStimulus(2)
      dut.dataIn.payload.zip(Seq(dataIn)).foreach { case (port, complex) => port #= complex }
      dut.preambleIn.payload.zip(Seq(preamble)).foreach { case (port, complex) => port #= complex }

      dut.clockDomain.waitSampling()
      (0 until 100).foreach { _ =>
        if (dut.counter.value.toInt == dut.period - 1) {
          dut.clockDomain.waitSampling()
          dutResult += dut.dataOut.payload.map(_.toComplex)
        }
        dut.clockDomain.waitSampling()
      }
      println(s"period: ${dut.period}")
      println(s"data: $dataIn")
      println(s"preamble: $preamble")
      println(dataIn / preamble)
      println(dutResult.head.head)
      assert(abs(dataIn / preamble - dutResult.head.head) < 1E-1)
    }
  }
}