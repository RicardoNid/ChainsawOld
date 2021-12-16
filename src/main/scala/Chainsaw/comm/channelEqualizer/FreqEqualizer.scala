package Chainsaw.comm.channelEqualizer

import Chainsaw._
import spinal.core._
import spinal.core.sim.SimConfig
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps


case class Equalization(dspType: HardType[SFix]) extends Component {

  val vecSize = 1
  val complexType = HardType(ComplexNumber(dspType))

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
  val dsp0s = Seq.fill(vecSize)(DSP48(dspType))
  val dsp1s = Seq.fill(vecSize)(DSP48(dspType))

  (dsp0s ++ dsp1s).foreach { dsp => // default value, will
    Seq(dsp.x, dsp.y, dsp.z, dsp.mode).foreach(_.allowOverride)
    dsp.idle()
  }

  dataOut.payload.zip(temp2.zip(temp3)).foreach { case (out, (real, imag)) =>
    out.real := real
    out.imag := imag
  }
  dataOut.valid := RegNext(dataIn.valid)

  def driveDSP(xs: Vec[SFix], ys: Vec[SFix], rets: Vec[SFix], dspIndex: Int, order: String): Unit = {
    val dsps = if (dspIndex == 0) dsp0s else dsp1s
    xs.zip(ys).zip(rets).zip(dsps).foreach { case (((x, y), ret), dsp) =>
      order match {
        case "mult" => dsp.mult(x, y, ret)
        case "add" => dsp.add(x, y, ret)
        case "sub" => dsp.sub(x, y, ret)
      }
    }
  }

  val mult0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = driveDSP(_, _, _, 0, "mult")
  val mult1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = driveDSP(_, _, _, 1, "mult")
  val add0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = driveDSP(_, _, _, 0, "add")
  val add1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = driveDSP(_, _, _, 1, "add")
  val sub0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = driveDSP(_, _, _, 0, "sub")

  val counter = CounterFreeRun(76)
  counter.value.simPublic()

  switch(counter.value) {
    is(U(0)) {
      mult0(preambleReal, preambleReal, tk) // real^2
      mult1(preambleImag, preambleImag, xk) // imag^2
      temp0 := preambleReal
      temp1 := preambleImag
    }
    is(U(1)) {
      sub0(dspZero, temp1, temp1)
      add1(xk, tk, tk) // now, tk = energy
    }
    is(U(2)) {
      mult0(tk, dspQuarter, tk)
      xk := dspOne // now, xk = 1
    }
    (0 until 10).foreach { i => // towords 1 / energy
      is(U(2 * i + 3))(sub0(dspTwo, tk, temp2))
      is(U(2 * i + 4)) {
        mult0(xk, temp2, xk)
        mult1(tk, temp2, tk)
      }
    } // after this, xk = 1 / energy
    is(U(23)) {
      mult0(xk, temp0, xk)
      mult1(xk, temp1, tk)
    }
    is(U(24)) {
      mult0(xk, dspQuarter, xk) // xk = factor.real
      mult1(tk, dspQuarter, tk) // tk = factor.imag
    }
    (0 until 16).foreach { i => // data* factor, (a+bj) * (c+dj)
      is(3 * i + 25) {
        mult0(dataReal, xk, temp0) // ac
        mult1(dataImag, tk, temp1) // bd
        temp2 := dataReal
        temp3 := dataImag
      }
      is(3 * i + 26) {
        mult0(temp2, tk, temp2) // ad
        mult1(temp3, xk, temp3) // bc
      }
      is(3 * i + 27) {
        sub0(temp0, temp1, temp2) // result.real
        add1(temp2, temp3, temp3) // result.imag
      }
    }
    default() // do nothing
  }
}

/** channel equalizer, mapping to Xilinx DSP slices
 *
 * @param golden    golden preamble sequence
 * @param iteration number of cycles we used for division
 */
case class FreqEqualizer(golden: Seq[Int], iteration: Int) extends Component {


}

object FreqEqualizer {
  def main(args: Array[String]): Unit = {
    //    VivadoSynth(Smooth(Seq.fill(vecSize)(-1), HardType(SFix(7 exp, 18 bits))))
    //    VivadoSynth(Equalization(HardType(SFix(7 exp, 18 bits))))

    SimConfig.withWave.compile(Equalization(SFix(7 exp, 18 bits))).doSim { dut =>
      val dutResult = ArrayBuffer[Seq[BComplex]]()
      val dataIn = ChainsawRand.nextComplex()
      val preamble = ChainsawRand.nextComplex()
      dut.clockDomain.forkStimulus(2)
      dut.dataIn.payload.zip(Seq(dataIn)).foreach { case (port, complex) => port #= complex }
      dut.preambleIn.payload.zip(Seq(preamble)).foreach { case (port, complex) => port #= complex }

      dut.clockDomain.waitSampling()
      (0 until 100).foreach { _ =>
        if (dut.counter.value.toInt == 75) {
          dut.clockDomain.waitSampling()
          dutResult += dut.dataOut.payload.map(_.toComplex)
        }
        dut.clockDomain.waitSampling()
      }
      println(s"data: $dataIn")
      println(s"preamble: $preamble")
      println(dataIn / preamble)
      println(dutResult.head.head)
    }

  }
}
