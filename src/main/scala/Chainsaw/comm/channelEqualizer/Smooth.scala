package Chainsaw.comm.channelEqualizer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class Smooth(golden: Seq[Int], dspType: HardType[SFix]) extends Component {

  def shiftLeft(vec: Vec[SFix], i: Int) = {
    val ret = cloneOf(vec)
    ret.zip(vec).foreach { case (r, v) => r := (v << i).truncated }
  }

  def divide2(vec: Vec[SFix]) = vec.zip(addSubs).foreach { case (v, dsp) => dsp.divide2(v, v) }

  def divide16(vec: Vec[SFix]) = vec.zip(addSubs).foreach { case (v, dsp) => dsp.divide16(v, v) }

  def add(as: Vec[SFix], bs: Vec[SFix], rets: Vec[SFix]) = {
    as.zip(bs).zip(rets).zip(addSubs).foreach { case (((a, b), ret), dsp) => dsp.add(a, b, ret) }
  }

  val complexType = HardType(ComplexNumber(dspType))
  val dspZero = dspType().getZero

  val dataIn = slave Flow Vec(complexType, 256) // preambles
  val dataOut = master Flow Vec(complexType, 256) // preambles after smooth
  val inReal = Vec(dataIn.payload.map(_.real))
  val inImag = Vec(dataIn.payload.map(_.imag))

  val reg0, reg1 = Reg(Vec(dspType(), 256))
  val srl0, srl1 = Reg(Vec(dspType(), 256 + 15))

  val counter = spinal.lib.CounterFreeRun(42)
  counter.value.simPublic()
  val counterSize = counter.getBitsWidth

  val addSubs = Seq.fill(256)(AddSub(dspType))
  // default value
  addSubs.indices.foreach { i =>
    val addSub = addSubs(i)
    addSub.mode := B"00"
    addSub.x := reg0(i)
    addSub.y := inReal(i)
    addSub.mode.allowOverride
    addSub.x.allowOverride
    addSub.y.allowOverride
  }

  switch(counter.value) {
    is(U(0, counterSize bits)) { // input
      reg0 := inReal
      reg1 := inImag
    }
    is(U(1, counterSize bits))(add(reg0, inReal, reg0)) // average of two preambles
    is(U(2, counterSize bits))(add(reg1, inImag, reg1))
    is(U(3, counterSize bits))(divide2(reg0))
    is(U(4, counterSize bits))(divide2(reg1))
    // adjust by golden
    is(U(5, counterSize bits))(reg0.zip(golden).zip(addSubs).foreach { case ((reg, i), dsp) => if (i != 1) dsp.negate(reg, reg) })
    is(U(6, counterSize bits))(reg1.zip(golden).zip(addSubs).foreach { case ((reg, i), dsp) => if (i != 1) dsp.negate(reg, reg) })
    // smooth
    // prepare data
    is(U(7, counterSize bits)) {
      srl0.take(7).foreach(_ := reg0.head)
      srl0.slice(7, 7 + 256).zip(reg0).foreach { case (srl, reg) => srl := reg }
      srl0.takeRight(8).foreach(_ := reg0.last)
      srl1.take(7).foreach(_ := reg1.head)
      srl1.slice(7, 7 + 256).zip(reg1).foreach { case (srl, reg) => srl := reg }
      srl1.takeRight(8).foreach(_ := reg1.last)

      reg0.foreach(_ := dspZero)
      reg1.foreach(_ := dspZero)
    }
    // accumulation
    (8 until 8 + 16).foreach(i =>
      is(U(i, counterSize bits)) {
        add(reg0, srl0, reg0)
        srl0.init.zip(srl0.tail).foreach { case (low, high) => low := high }
      }
    )
    (24 until 24 + 16).foreach { i =>
      is(U(i, counterSize bits)) {
        add(reg1, srl1, reg1)
        srl1.init.zip(srl1.tail).foreach { case (low, high) => low := high }
      }
    }
    is(U(40, counterSize bits))(divide16(reg0))
    is(U(41, counterSize bits))(divide16(reg1))
    default() // do nothing, all regs keep their values
  }

  dataOut.payload.zip(reg0.zip(reg1)).foreach { case (out, (real, imag)) => out.real := real; out.imag := imag }
  dataOut.valid := RegNext(dataIn.valid)
}
