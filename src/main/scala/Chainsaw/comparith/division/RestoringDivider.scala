package Chainsaw.comparith.division

import spinal.lib._
import Chainsaw._
import Chainsaw.comparith._
import spinal.core._
import scala.collection._
import spinal.core.sim._

// the restoringDivider algorithm
case class RestoringDivider(config: DividerConfig) extends Component {
  val io = new Bundle {
    val dividend = in SInt (2 * config.width + 1 bits)
    val divisor = in SInt (config.width + 1 bits)
    val quotient = out SInt (config.width + 1 bits)
    val remainder = out SInt (config.width + 1 bits)
  }

  // preprocess the dividend and divisor
  val dividendSign = if (config.signed) io.dividend.msb else False
  val divisorSign = if (config.signed) io.divisor.msb else False

  val uintDividend = UInt(2 * config.width + 1 bits)
  switch(dividendSign) {
    is(True) {
      uintDividend := (io.dividend.asBits ^ B(2 * config.width + 1 bits, default -> true)).asUInt + U(1)
    }
    is(False) {
      uintDividend := io.dividend.asBits.asUInt
    }
  }

  val uintDivisor = UInt(config.width + 1 bits)
  switch(divisorSign) {
    is(True) {
      uintDivisor := (io.divisor.asBits ^ B(config.width + 1 bits, default -> true)).asUInt + U(1)
    }
    is(False) {
      uintDivisor := io.divisor.asBits.asUInt
    }
  }

  val dividendReg = Reg(UInt(2 * config.width + 1 bits)) init (uintDividend)
  val quotientReg = Reg(UInt(config.width + 1 bits)) init (U(0, config.width + 1 bits))

  io.remainder := dividendReg(config.width, config.width + 1 bits).asSInt
  io.quotient := quotientReg.asSInt


  // define other signal which the compute need
  val counter = Counter(0, config.width + 1)
  val holdResult = False
  holdResult.noCombLoopCheck

  // begin compute
  when(counter <= U(config.width - 1)) {
    val adder = BasicAdder(config.width, AdderType.RCA).setDefinitionName("Adder")
    adder.setName("adderForIteration")
    adder.x := dividendReg(config.width - 1, config.width bits)
    adder.y := (uintDivisor(0, config.width bits).asBits ^ B(config.width bits, default -> true)).asUInt
    adder.cIn := U(1, 1 bits)

    when(adder.fullSum.msb | dividendReg(2 * config.width - 1, 1 bits).asBool) {
      dividendReg := (!(dividendReg(2 * config.width - 1, 1 bits).asBool | adder.fullSum.msb) ## adder.fullSum(0, config.width bits) ## dividendReg(0, config.width - 1 bits) ## B(0, 1 bits)).asUInt
    } otherwise {
      dividendReg := (dividendReg(0, 2 * config.width bits) ## B(0, 1 bits)).asUInt
    }
    quotientReg := (quotientReg(0, config.width bits) ## (adder.fullSum.msb | dividendReg(2 * config.width - 1, 1 bits).asBool)).asUInt
    counter.increment()
  }
    // adjust quotient and remainder when the last partial remainder is negative
    .elsewhen(counter === U(config.width)) {
      when(dividendReg.msb) {
        val adderForRemainder, adderForQuotient = new BasicAdder(config.width + 1, AdderType.RCA).setDefinitionName("adderForResult")
        adderForRemainder.setName("adderForRemainder")
        adderForQuotient.setName("adderForQuotient")
        adderForRemainder.x := dividendReg(config.width, config.width + 1 bits)
        adderForRemainder.y := uintDivisor(0, config.width + 1 bits)
        adderForRemainder.cIn := U(0, 1 bits)
        switch(dividendSign) {
          is(True) {
            dividendReg(config.width, config.width + 1 bits) := (adderForRemainder.fullSum(0, config.width + 1 bits).asBits ^ B(config.width + 1 bits, default -> true)).asUInt + U(1)
          }
          is(False) {
            dividendReg(config.width, config.width + 1 bits) := adderForRemainder.fullSum(0, config.width + 1 bits)
          }
        }
        adderForQuotient.x := quotientReg
        adderForQuotient.y := (U(1, config.width + 1 bits).asBits ^ B(config.width + 1 bits, default -> true)).asUInt
        adderForQuotient.cIn := U(1, 1 bits)
        switch(dividendSign ^ divisorSign) {
          is(True) {
            quotientReg := (adderForQuotient.fullSum(0, config.width + 1 bits).asBits ^ B(config.width + 1 bits, default -> true)).asUInt + U(1)
          }
          is(False) {
            quotientReg := adderForQuotient.fullSum(0, config.width + 1 bits)
          }
        }
        counter.increment()
      } otherwise {
        switch(dividendSign) {
          is(True) {
            dividendReg(config.width, config.width + 1 bits) := (dividendReg(config.width, config.width + 1 bits).asBits ^ B(config.width + 1 bits, default -> true)).asUInt + U(1)
          }
          is(False) {
            dividendReg(config.width, config.width + 1 bits) := dividendReg(config.width, config.width + 1 bits)
          }
        }
        switch(dividendSign ^ divisorSign) {
          is(True) {
            quotientReg := (quotientReg.asBits ^ B(config.width + 1 bits, default -> true)).asUInt + U(1)
          }
          is(False) {
            quotientReg := quotientReg
          }
        }
        counter.increment()
      }
    } otherwise {

  }
}


import spinal.sim._


object TestRestoringDivider extends App {
  SimConfig.withWave.allOptimisation.compile(RestoringDivider(DividerConfig(true, 4)))
    .doSim { dut =>
      import dut.{clockDomain, io}
      clockDomain.forkStimulus(10)
      clockDomain.assertReset()
      io.dividend #= -49
      io.divisor #= -7

      clockDomain.deassertReset()
      if (dut.config.signed) {
        clockDomain.waitSampling(dut.config.width + 2)
        println(s"the devidend is : ${io.dividend.toInt}")
        println(s"the divisor is : ${io.divisor.toInt}")
        println(s"the quotient is : ${io.quotient.toInt}")
        println(s"the remainder is : ${io.remainder.toInt}")
      } else {
        clockDomain.waitSampling(dut.config.width + 1)
        println(s"the devidend is : ${io.dividend.toInt}")
        println(s"the divisor is : ${io.divisor.toInt}")
        println(s"the quotient is : ${io.quotient.toInt}")
        println(s"the remainder is : ${io.remainder.toInt}")
      }

    }
  //  SpinalVerilog(new restoringDivider(DividerConfig(true, 4)))
}
