package Chainsaw.comparith.division

import Chainsaw.comparith._
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.math._

class Radix2SRTDivider(val config: DividerConfig) extends Component {
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

  // normalize dividend and divisor
  val normalizedDividend = uintDividend
  val normalizedDivisor = U(0, config.width + 1 bits)
  // if this signal is Ture we should take a cycle to adjust the result after we complete the k cycle iterations
  val isInitDividendOver = normalizedDividend(2 * config.width - 1, 1 bits).asBool
  val initDivisorShiftNumber = U(0, log2Up(config.width - 1) bits)

  for (i <- 0 until config.width) {
    when(U(pow(2, i + 1).toInt) > uintDivisor & uintDivisor >= U(pow(2, i).toInt)) {
      normalizedDivisor := uintDivisor.rotateLeft(config.width - 1 - i)
      initDivisorShiftNumber := U(config.width - 1 - i).resized
    }
  }

  val dividendReg = Reg(UInt(2 * config.width + 1 bits)) init (normalizedDividend)
  // use two register to restore the positive and negative quotient digit respectively
  val positiveQuotientReg = Reg(UInt(config.width + 1 bits)) init (U(0, config.width + 1 bits))
  val negativeQuotientReg = Reg(UInt(config.width + 1 bits)) init (U(0, config.width + 1 bits))

  val counter = Counter(0, config.width + 4)

  // decide the quotient value
  val quotientValue = Bits(2 bits)
  when(!dividendReg(2 * config.width - 1, 1 bits).asBool & dividendReg(2 * config.width - 2, 1 bits).asBool) {
    quotientValue := B("2'01")
  }.elsewhen(dividendReg(2 * config.width - 1, 1 bits).asBool & !dividendReg(2 * config.width - 2, 1 bits).asBool) {
    quotientValue := B("2'11")
  } otherwise {
    quotientValue := B("2'00")
  }

  io.remainder := dividendReg(config.width, config.width + 1 bits).asSInt
  // we restore the final result to the positive quotient register
  io.quotient := positiveQuotientReg.asSInt

  when(counter <= U(config.width - 1)) {
    when(quotientValue === B("2'01")) {
      val adder = BasicAdder(config.width + 1, AdderType.RCA)
      adder.setName("adderForSubIterations")
      adder.x := dividendReg(config.width - 1, config.width + 1 bits)
      adder.y := (normalizedDivisor.asBits ^ B(config.width + 1 bits, default -> true)).asUInt
      adder.cIn := U(1, 1 bits)
      dividendReg := adder.fullSum(0, config.width + 1 bits) @@ dividendReg(0, config.width - 1 bits) @@ U(0, 1 bits)
      positiveQuotientReg := positiveQuotientReg(0, config.width bits) @@ U(1, 1 bits)
      negativeQuotientReg := negativeQuotientReg(0, config.width bits) @@ U(0, 1 bits)
      counter.increment()
    }.elsewhen(quotientValue === B("2'11")) {
      val adder = BasicAdder(config.width + 1, AdderType.RCA)
      adder.setName("adderForAddIterations")
      adder.x := dividendReg(config.width - 1, config.width + 1 bits)
      adder.y := normalizedDivisor
      adder.cIn := U(0, 1 bits)
      dividendReg := adder.fullSum(0, config.width + 1 bits) @@ dividendReg(0, config.width - 1 bits) @@ U(0, 1 bits)
      positiveQuotientReg := positiveQuotientReg(0, config.width bits) @@ U(0, 1 bits)
      negativeQuotientReg := negativeQuotientReg(0, config.width bits) @@ U(1, 1 bits)
      counter.increment()
    } otherwise {
      dividendReg := dividendReg(0, 2 * config.width bits) @@ U(0, 1 bits)
      positiveQuotientReg := positiveQuotientReg(0, config.width bits) @@ U(0, 1 bits)
      negativeQuotientReg := negativeQuotientReg(0, config.width bits) @@ U(0, 1 bits)
      counter.increment()
    }
  }.elsewhen(counter === U(config.width)) {
    val adderForQuotientConvert = BasicAdder(config.width + 1, AdderType.RCA).setDefinitionName("adderForResult")

    when(dividendReg.msb =/= normalizedDividend.msb) {
      val adderForRemainder, adderForQuotientAdjust = BasicAdder(config.width + 1, AdderType.RCA).setDefinitionName("adderForResult")
      adderForRemainder.setName("adderForRemainder")
      adderForQuotientConvert.setName("adderForQuotientConvert")
      adderForQuotientAdjust.setName("adderForQuotientAdjust")

      adderForRemainder.x := dividendReg(config.width, config.width + 1 bits)
      adderForRemainder.y := normalizedDivisor
      adderForRemainder.cIn := U(0, 1 bits)

      dividendReg := adderForRemainder.fullSum(0, config.width + 1 bits) @@ dividendReg(0, config.width bits)

      adderForQuotientConvert.x := positiveQuotientReg
      adderForQuotientConvert.y := (negativeQuotientReg.asBits ^ B(config.width + 1 bits, default -> true)).asUInt
      adderForQuotientConvert.cIn := U(1, 1 bits)

      adderForQuotientAdjust.x := adderForQuotientConvert.fullSum(0, config.width + 1 bits)
      adderForQuotientAdjust.y := (B(1, config.width + 1 bits) ^ B(config.width + 1 bits, default -> true)).asUInt
      adderForQuotientAdjust.cIn := U(1, 1 bits)

      negativeQuotientReg := negativeQuotientReg
      positiveQuotientReg := adderForQuotientAdjust.fullSum(0, config.width + 1 bits)

      counter.increment()
    } otherwise {
      dividendReg := dividendReg

      adderForQuotientConvert.x := positiveQuotientReg
      adderForQuotientConvert.y := (negativeQuotientReg.asBits ^ B(config.width + 1 bits, default -> true)).asUInt
      adderForQuotientConvert.cIn := U(1, 1 bits)

      positiveQuotientReg := adderForQuotientConvert.fullSum(0, config.width + 1 bits)
      negativeQuotientReg := negativeQuotientReg
      counter.increment()
    }

  }.elsewhen(counter === U(config.width + 1)) {
    // compensate the dividend and quotient
    when(isInitDividendOver | initDivisorShiftNumber =/= U(0)) {
      val compensateFactor = isInitDividendOver.asUInt + initDivisorShiftNumber
      dividendReg := dividendReg
      positiveQuotientReg := (U(2) * compensateFactor * positiveQuotientReg(0, config.width + 1 bits)).resize(config.width + 1)
      counter.increment()
    } otherwise {
      dividendReg := dividendReg
      positiveQuotientReg := positiveQuotientReg
      counter.increment()
    }
  }.elsewhen(counter === U(config.width + 2)) {
    val interimDividend = dividendReg
    val interimQuotient = positiveQuotientReg

    // fix final quotient and remainder
    when(interimDividend(config.width, config.width + 1 bits) >= uintDivisor) {
      interimDividend := (interimDividend(config.width, config.width + 1 bits) - uintDivisor) @@ interimDividend(0, config.width bits)
      interimQuotient := interimQuotient + U(1, config.width + 1 bits)
    }
    counter.increment()
  }.elsewhen(counter === U(config.width + 3)) {
    switch(dividendSign) {
      is(True) {
        dividendReg := (dividendReg.asBits ^ B(2 * config.width + 1 bits, default -> true)).asUInt + U(1)
      }
      is(False) {
        dividendReg:= dividendReg
      }
    }
    switch(dividendSign ^ divisorSign) {
      is(True) {
        positiveQuotientReg := (positiveQuotientReg.asBits ^ B(config.width + 1 bits, default -> true)).asUInt + U(1)
      }
      is(False) {
        positiveQuotientReg := positiveQuotientReg
      }
    }
    counter.increment()
  } otherwise {

  }

}

object TestRadix2SRTDivider extends App {
  SimConfig.withWave.allOptimisation.compile(new Radix2SRTDivider(DividerConfig(true, 4)))
    .doSim { dut =>
      import dut.{clockDomain, io}
      clockDomain.forkStimulus(10)
      clockDomain.assertReset()
      io.dividend #= -49
      io.divisor #= 7

      clockDomain.deassertReset()
      clockDomain.waitSampling(dut.config.width + 5)
      println(s"the devidend is : ${io.dividend.toInt}")
      println(s"the divisor is : ${io.divisor.toInt}")
      println(s"the quotient is : ${io.quotient.toInt}")
      println(s"the remainder is : ${io.remainder.toInt}")


    }
}