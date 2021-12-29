package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class EqualizationFTN()
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]]{

  println(s"equalization FTN period: $latency")
  require(latency <= 80)

  val preambleIn = slave Stream equalizerComplexVecType() // preambles after smooth
  val dataIn = slave Stream equalizerComplexVecType() // data before equalization
  val dataOut = master Stream equalizerComplexVecType() // data after equalization

  val iteration = 12
  val latency = 2 * iteration + 3 * 16 + 3 + 2 + 1

  val preambleReal = Vec(preambleIn.payload.map(_.real))
  val preambleImag = Vec(preambleIn.payload.map(_.imag))
  val dataReal = Vec(dataIn.payload.map(_.real))
  val dataImag = Vec(dataIn.payload.map(_.imag))

  val dspZero = Vec(equalizerType().getZero, equalizerWidth)
  def dspConstant(constant:Double) = Vec(SFLike(constant, equalizerType), equalizerWidth)
  val dspOne = dspConstant(1.0)
  val dspTwo = dspConstant(2.0)
  val dspQuarter = dspConstant(0.25)

  val tk, xk, temp0, temp1, temp2 = Reg(equalizerVecType) // registers

  val dsps = Seq.fill(2)(VecMult(equalizerVecType))
  val Seq(dsp0, dsp1) = dsps
  val Seq(prod0, prod1) = dsps.map(_.rets)
  dsps.foreach(_.init())

  val addSubs = Seq.fill(2)(VecAddSub(equalizerVecType))
  val Seq(addSub0, addSub1) = addSubs
  addSubs.foreach(_.init())

  dataOut.payload.zip(temp0.zip(temp1)).foreach { case (out, (real, imag)) =>
    out.real := real
    out.imag := imag
  }

  val mult0: (Vec[SFix], Vec[SFix]) => Unit = dsp0.mult
  val mult1: (Vec[SFix], Vec[SFix]) => Unit = dsp1.mult
  val add0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub0.add
  val add1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub1.add
  val sub0: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub0.sub
  val sub1: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = addSub1.sub

  // TODO: remove this in final version
  val counter = Counter(latency)
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
        case entry: State with EntryPoint => entry.whenIsActive(when(preambleIn.fire)(goto(next))) // starts when fire
        case delay: StateDelay => delay.whenCompleted(goto(next))
        case _ => prev.whenIsActive(goto(next))
      }
      prev.whenIsActive(counter.increment())
    }

    preambleIn.ready := isActive(GETENERGY0)
    dataIn.ready := isActive(EQUALIZE) && complexMultCounter.value === U(0)

    val produce = False
    dataOut.valid := RegNext(produce, init = False)

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
        produce := True
      }
    }

    val badPreamble = !preambleIn.fire && isActive(GETENERGY0)
    val badData = !dataIn.fire && isActive(EQUALIZE)
    Seq(badPreamble, badData).foreach(_.simPublic())
  }
}
