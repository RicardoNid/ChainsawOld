package Chainsaw.comm.channelEqualizer

import Chainsaw._
import breeze.numerics.abs
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

case class ComplexMultSubroutine(dspType: HardType[SFix]) extends Component {

  val complexType = HardType(ComplexNumber(dspType))
  val x, y = in(complexType())
  val z = out(complexType())

  val counter = CounterFreeRun(3)
  counter.value.simPublic()
  val temp0, temp1 = Reg(dspType())

  val i0 = -x.imag
  val i1 = -y.imag

  switch(counter.value) {
    is(U(0))(temp0 := ((x.real + i0) * y.real).truncated) // e
    is(U(1)) { // i
      val temp = dspType()
      temp := ((y.real + y.imag) * x.real).truncated
      temp1 := temp - temp0
    }
    is(U(2)) {
      val temp = dspType()
      temp := ((y.real + i1) * x.imag).truncated
      temp0 := temp + temp0
    }
    default()
  }

  z.real := temp0
  z.imag := temp1
}

object ComplexMultSubroutine {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(ComplexMultSubroutine(HardType(SFix(7 exp, 18 bits)))).doSim { dut =>
      val dutResult = ArrayBuffer[BComplex]()
      val x, y = ChainsawRand.nextComplex()
      dut.x #= x
      dut.y #= y
      dut.clockDomain.forkStimulus(2)
      dut.clockDomain.waitSampling()
      (0 until 4).foreach { _ =>
        if (dut.counter.value.toInt == 2) {
          dut.clockDomain.waitSampling()
          dutResult += dut.z.toComplex
        }
        dut.clockDomain.waitSampling()
      }

      assert(abs(x * y - dutResult.head) < 1E-2)
    }

    VivadoSynth(ComplexMultSubroutine(HardType(SFix(7 exp, 18 bits))))
  }
}
