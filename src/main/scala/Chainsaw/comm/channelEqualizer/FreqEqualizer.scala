package Chainsaw.comm.channelEqualizer

import Chainsaw._
import breeze.numerics.abs
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._
import spinal.lib.fsm._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class VecAddSub(dspType: HardType[SFix], vecSize: Int) extends Component {
  val xs, ys = in(Vec(dspType(), vecSize))
  val mode = in Bits (1 bits)
  mode.addAttribute("max_fanout", 32)
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

  rets.zip(xs.zip(ys)).foreach { case (ret, (x, y)) => ret := (x * y).truncated }

  def drive(xOuter: Vec[SFix], yOuter: Vec[SFix], retOuter: Vec[SFix]) = {
    Seq(xOuter, yOuter).zip(Seq(xs, ys)).foreach { case (signal, port) =>
      if (signal == null) port.assignDontCare() else port := signal
    }
    if (retOuter != null) retOuter := rets
  }

  val mult: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = drive(_, _, _)

  def idle() = drive(null, null, null)

  def init() = {
    Seq(xs, ys).foreach(_.allowOverride)
    idle()
  }

}

case class Equalization(dspType: HardType[SFix], vecSize:Int) extends Component {

  val complexType = HardType(ComplexNumber(dspType))
  val iteration = 12
  val period = 2 * iteration + 3 * 16 + 3 + 2
  require(period <= 80)

  val preambleIn = slave Flow Vec(complexType, vecSize) // preambles after smooth
  val dataIn = slave Flow Vec(complexType, vecSize) // data before equalization
  val preambleReal = Vec(preambleIn.payload.map(_.real))
  val preambleImag = Vec(preambleIn.payload.map(_.imag))
  val dataReal = Vec(dataIn.payload.map(_.real))
  val dataImag = Vec(dataIn.payload.map(_.imag))

  val dataOut = master Flow Vec(complexType, vecSize) // data after equalization

  val dspZero = Vec(dspType().getZero, vecSize)
  val dspOne = Vec(SFLike(1.0, dspType()), vecSize)
  val dspTwo = Vec(SFLike(2.0, dspType()), vecSize)
  val dspQuarter = Vec(SFLike(0.25, dspType()), vecSize)

  val tk, xk, temp0, temp1, temp2, temp3 = Reg(Vec(dspType(), vecSize)) // registers

  val dsp0, dsp1 = VecMult(dspType, vecSize)
  val addSub0, addSub1 = VecAddSub(dspType, vecSize)

  Seq(addSub0, addSub1).foreach(_.init())
  Seq(dsp0, dsp1).foreach(_.init())

  dataOut.payload.zip(temp2.zip(temp3)).foreach { case (out, (real, imag)) =>
    out.real := real
    out.imag := imag
  }
  dataOut.valid := RegNext(dataIn.valid)

  val mult0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = dsp0.mult
  val mult1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = dsp1.mult
  val add0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub0.add
  val add1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub1.add
  val sub0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub0.sub
  val sub1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub1.sub

  // TODO: remove this in final version
  val counter = Counter(period)
  counter.value.simPublic()

  val dbcCounter = Counter(2)
  dbcCounter.value.simPublic()
  val complexMultCounter = Counter(3)
  complexMultCounter.value.simPublic()

  val fsm = new StateMachine {
    val GETENERGY0 = StateEntryPoint()
    val GETENERGY1, GETENERGY2, GETFACTOR0, GETFACTOR1 = new State()
    val DBC = new StateDelay(iteration * 2)
    val EQUALIZE = new StateDelay(48)

    //  state transition logic
    val ordered = Seq(GETENERGY0, GETENERGY1, GETENERGY2, DBC, GETFACTOR0, GETFACTOR1, EQUALIZE)
    ordered.zip(ordered.tail :+ GETENERGY0).foreach { case (prev, next) =>
      prev match {
        case delay: StateDelay => delay.whenCompleted(goto(next))
        case _ => prev.whenIsActive(goto(next))
      }
      prev.whenIsActive(counter.increment())
    }

    // state behavior
    GETENERGY0.whenIsActive {
      mult0(preambleReal, preambleReal, tk) // real^2
      mult1(preambleImag, preambleImag, xk) // imag^2
      temp0 := preambleReal
      temp1 := preambleImag
    }

    GETENERGY1.whenIsActive {
      add0(tk, xk, tk) // now, tk = energy
      sub1(dspZero, temp1, temp1)
    }

    GETENERGY2.whenIsActive {
      mult0(tk, dspQuarter, tk)
      xk := dspOne // now, xk = 1
    }

    DBC.whenIsActive { // towards 1 / energy
      dbcCounter.increment()
      when(!dbcCounter.value.lsb)(sub0(dspTwo, tk, temp2))
        .otherwise {
          mult0(tk, temp2, tk)
          mult1(xk, temp2, xk)
        }
    } // after this, xk = 1 / energy

    GETFACTOR0.whenIsActive {
      mult0(xk, temp1, tk)
      mult1(xk, temp0, xk)
    }

    GETFACTOR1.whenIsActive {
      mult0(tk, dspQuarter, tk) // tk = factor.imag
      mult1(xk, dspQuarter, xk) // xk = factor.real
    }

    EQUALIZE.whenIsActive { // data* factor, (a+bj) * (c+dj)
      complexMultCounter.increment()
      when(complexMultCounter.value === U(0)) {
        mult0(xk, dataReal, temp0) // ac
        mult1(tk, dataImag, temp1) // bd
        temp2 := dataImag
        temp3 := dataReal
      }
        .elsewhen(complexMultCounter.value === U(1)) {
          mult0(xk, temp2, temp2) // bc
          mult1(tk, temp3, temp3) // ab
        }
        .otherwise {
          sub0(temp0, temp1, temp2) // result.real
          add1(temp2, temp3, temp3) // result.imag
        }
    }
  }
}

/** channel equalizer, mapping to Xilinx DSP slices
 *
 * @param golden    golden preamble sequence
 * @param iteration number of cycles we used for division
 */
case class FreqEqualizer(golden: Seq[Int], iteration: Int) extends Component {


}

object Equalization {
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
      println(s"data: $dataIn")
      println(s"preamble: $preamble")
      println(dataIn / preamble)
      println(dutResult.head.head)
      assert(abs(dataIn / preamble - dutResult.head.head) < 1E-1)
    }

    VivadoSynth(Equalization(HardType(SFix(7 exp, 18 bits)), 256))

  }
}