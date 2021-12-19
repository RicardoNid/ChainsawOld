package Chainsaw.comm.channelEqualizer

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class Equalization(dspType: HardType[SFix], vecSize: Int) extends Component {

  val complexType = HardType(ComplexNumber(dspType))
  val iteration = 12
  val period = 2 * iteration + 3 * 16 + 3 + 2 + 1
  require(period <= 80)

  val preambleIn = slave Flow Vec(complexType, vecSize) // preambles after smooth
  val preambleReal = Vec(preambleIn.payload.map(_.real))
  val preambleImag = Vec(preambleIn.payload.map(_.imag))
  val dataIn = slave Flow Vec(complexType, vecSize) // data before equalization
  val dataReal = Vec(dataIn.payload.map(_.real))
  val dataImag = Vec(dataIn.payload.map(_.imag))

  val preambleFIFO = StreamFifo(Vec(complexType, vecSize), 2)
  val dataFIFO = StreamFifo(Vec(complexType, vecSize), 16)

  val dataOut = master Flow Vec(complexType, vecSize) // data after equalization

  val dspZero = Vec(dspType().getZero, vecSize)
  val dspOne = Vec(SFLike(1.0, dspType()), vecSize)
  val dspTwo = Vec(SFLike(2.0, dspType()), vecSize)
  val dspQuarter = Vec(SFLike(0.25, dspType()), vecSize)

  val tk, xk, temp0, temp1, temp2 = Reg(Vec(dspType(), vecSize)) // registers

  val dsps = Seq.fill(2)(VecMult(dspType, vecSize))
  val Seq(dsp0, dsp1) = dsps
  val Seq(prod0, prod1) = dsps.map(_.rets)
  dsps.foreach(_.init())

  val addSubs = Seq.fill(2)(VecAddSub(dspType, vecSize))
  val Seq(addSub0, addSub1) = addSubs
  addSubs.foreach(_.init())

  dataOut.payload.zip(temp0.zip(temp1)).foreach { case (out, (real, imag)) =>
    out.real := real
    out.imag := imag
  }
  dataOut.valid := RegNext(dataIn.valid)

  val mult0: (Vec[SFix], Vec[SFix]) => Unit = dsp0.mult
  val mult1: (Vec[SFix], Vec[SFix]) => Unit = dsp1.mult
  val add0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub0.add
  val add1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub1.add
  val sub0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub0.sub
  val sub1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub1.sub

  // TODO: remove this in final version
  val counter = Counter(period)
  counter.value.simPublic()

  val dbcCounter = Counter(iteration * 2)
  dbcCounter.value.simPublic()
  val complexMultCounter = Counter(3)
  complexMultCounter.value.simPublic()

  val fsm = new StateMachine {
    val GETENERGY0 = StateEntryPoint()
    val GETENERGY1, GETENERGY2, GETFACTOR0, GETFACTOR1, SAVE = new State()
    val DBC = new StateDelay(iteration * 2)
    val EQUALIZE = new StateDelay(48)

    //  state transition logic
    val ordered = Seq(GETENERGY0, GETENERGY1, GETENERGY2, DBC, GETFACTOR0, GETFACTOR1, SAVE, EQUALIZE)
    ordered.zip(ordered.tail :+ GETENERGY0).foreach { case (prev, next) =>
      prev match {
        case delay: StateDelay => delay.whenCompleted(goto(next))
        case _ => prev.whenIsActive(goto(next))
      }
      prev.whenIsActive(counter.increment())
    }

    // state behavior
    GETENERGY0.whenIsActive {
      mult0(preambleReal, preambleReal) // real^2
      mult1(preambleImag, preambleImag) // imag^2
      temp0 := preambleReal
      temp1 := preambleImag
    }

    GETENERGY1.whenIsActive {
      add0(prod0, prod1, tk) // now, tk = energy
      sub1(dspZero, temp1, temp1)
    }

    GETENERGY2.whenIsActive {
      mult0(tk, dspQuarter) // prod0 = energy after norm
      xk := dspOne // now, xk = 1
    }

    DBC.whenIsActive { // towards 1 / energy
      dbcCounter.increment()
      when(!dbcCounter.value.lsb) {
        sub0(dspTwo, prod0, temp2)
        tk := prod0
        when(dbcCounter.value =/= U(0))(xk := prod1)
      }.otherwise {
        when(dbcCounter.value =/= U(iteration * 2 - 1))(mult0(tk, temp2))
          .otherwise(mult0(xk, temp2))
        mult1(xk, temp2)
      }
    } // after this, xk = 1 / energy

    GETFACTOR0.whenIsActive {
      mult0(prod0, temp1)
      mult1(prod1, temp0)
    }

    GETFACTOR1.whenIsActive {
      mult0(prod0, dspQuarter) // tk = factor.imag
      mult1(prod1, dspQuarter) // xk = factor.real
    }

    SAVE.whenIsActive {
      tk := prod0
      xk := prod1
    }

    EQUALIZE.whenIsActive { // data* factor, (a+bj) * (c+dj)
      complexMultCounter.increment()
      when(complexMultCounter.value === U(0)) {
        mult0(tk, dataImag) // bd
        mult1(xk, dataReal) // ac
        temp0 := dataImag
        temp1 := dataReal
      }.elsewhen(complexMultCounter.value === U(1)) {
        mult0(tk, temp1) // ab
        mult1(xk, temp0) // bc
        sub0(prod1, prod0, temp0) // result.real = ac - bd
      }.otherwise {
        add1(prod0, prod1, temp1) // result.imag
      }
    }
  }
}
