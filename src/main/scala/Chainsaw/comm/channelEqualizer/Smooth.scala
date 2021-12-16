package Chainsaw.comm.channelEqualizer

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/**
 * @param golden
 * @param dspType
 */
case class Smooth(golden: Seq[Int], dspType: HardType[SFix], vecSize: Int) extends Component {

  def shiftRight(vec: Vec[SFix], i: Int) = vec.foreach(v => v := (v >> i).truncated)

  def vecAdd(xs: Vec[SFix], ys: Vec[SFix], rets: Vec[SFix]) = xs.zip(ys).zip(rets).foreach { case ((x, y), ret) => ret := x + y }

  def adjust(vec: Vec[SFix]) = vec.zip(golden).foreach { case (reg, i) => if (i != 1) reg := -reg }

  val complexType = HardType(ComplexNumber(dspType))
  val dspZero = dspType().getZero

  val dataIn = slave Flow Vec(complexType, vecSize) // preambles
  val dataOut = master Flow Vec(complexType, vecSize) // preambles after smooth
  val inReal = Vec(dataIn.payload.map(_.real))
  val inImag = Vec(dataIn.payload.map(_.imag))

  val reg0, reg1 = Reg(Vec(dspType(), vecSize))
  val srl0, srl1 = Reg(Vec(dspType(), vecSize + 15))

  val period = 3 + 1 + 1 + 16
  println(s"period: $period")

  val counter = spinal.lib.Counter(period)
  counter.value.simPublic()
  val counterSize = counter.getBitsWidth

  val fsm = new StateMachine {
    val GETAVERAGE0 = StateEntryPoint()
    val GETAVERAGE1, GETAVERAGE2, GOLDEN, PREPARE = new State()
    val SMOOTH = new StateDelay(16)
    val smoothCounter = spinal.lib.Counter(16)
    val ordered = Seq(GETAVERAGE0, GETAVERAGE1, GETAVERAGE2, GOLDEN, PREPARE, SMOOTH)
    ordered.zip(ordered.tail :+ GETAVERAGE0).foreach { case (prev, next) =>
      prev match {
        case delay: StateDelay => delay.whenCompleted(goto(next))
        case _ => prev.whenIsActive(goto(next))
      }
      prev.whenIsActive(counter.increment())
    }
    GETAVERAGE0.whenIsActive {
      reg0 := inReal
      reg1 := inImag
    }
    GETAVERAGE1.whenIsActive {
      vecAdd(reg0, inReal, reg0)
      vecAdd(reg1, inImag, reg1)
    }
    GETAVERAGE2.whenIsActive {
      shiftRight(reg0, 1)
      shiftRight(reg1, 1)
    }
    GOLDEN.whenIsActive {
      adjust(reg0)
      adjust(reg1)
    }
    PREPARE.whenIsActive {
      srl0.take(7).foreach(_ := reg0.head)
      srl0.slice(7, 7 + vecSize).zip(reg0).foreach { case (srl, reg) => srl := reg }
      srl0.takeRight(8).foreach(_ := reg0.last)
      srl1.take(7).foreach(_ := reg1.head)
      srl1.slice(7, 7 + vecSize).zip(reg1).foreach { case (srl, reg) => srl := reg }
      srl1.takeRight(8).foreach(_ := reg1.last)

      reg0.foreach(_ := dspZero)
      reg1.foreach(_ := dspZero)
    }
    SMOOTH.whenIsActive {
      smoothCounter.increment()
      vecAdd(reg0, Vec(srl0.take(256)), reg0) // accumulation
      vecAdd(reg1, Vec(srl1.take(256)), reg1)
      srl0.init.zip(srl0.tail).foreach { case (low, high) => low := high } // shifting(or, sliding)
      srl1.init.zip(srl1.tail).foreach { case (low, high) => low := high }
    }
  }

  dataOut.payload.zip(reg0.zip(reg1)).foreach { case (out, (real, imag)) =>
    out.real := (real >> 4).truncated
    out.imag := (imag >> 4).truncated
  }
  dataOut.valid := RegNext(dataIn.valid)
}


