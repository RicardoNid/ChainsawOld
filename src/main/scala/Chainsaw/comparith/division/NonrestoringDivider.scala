package Chainsaw.comparith.division

import spinal.lib._
import Chainsaw._
import Chainsaw.comparith._
import spinal.core._
import scala.collection._
import spinal.core.sim._

class NonRestoringDivider(val config: DividerConfig) extends Component {
  val io = new Bundle {
    val dividend  = in SInt (2 * config.width + 1 bits)
    val divisor   = in SInt (config.width + 1 bits)
    val quotient  = out SInt (config.width + 1 bits)
    val remainder = out SInt (config.width + 1 bits)
  }

  // register the interim result
  val dividendReg = Reg(SInt(2 * config.width + 1 bits)) init (io.dividend)
  val quotientReg = Reg(SInt(config.width + 1 bits)) init (S(0, config.width + 1 bits))

  // decide add or subtract
  val addOrSub = dividendReg.msb ^ io.divisor.msb

  // output logic
  io.quotient  := quotientReg
  io.remainder := dividendReg(config.width, config.width + 1 bits)

  // define other signal we need
  val counter = Counter(0, config.width + 2)
  // begin compute
  when(counter <= U(config.width - 1)) {
    val adder = BasicAdder(config.width, AdderType.RCA).setDefinitionName("Adder")
    adder.setName("adderForIteration")
    // according to addOrSub to do operation
    when(addOrSub) {
      adder.x   := dividendReg(config.width - 1, config.width bits).asUInt
      adder.y   := io.divisor(0, config.width bits).asUInt
      adder.cIn := U(0, 1 bits)
    } otherwise {
      adder.x   := dividendReg(config.width - 1, config.width bits).asUInt
      adder.y   := (io.divisor(0, config.width bits).asBits ^ B(config.width bits, default -> true)).asUInt
      adder.cIn := U(1, 1 bits)
    }
    // refresh quotient and partial remainder
    quotientReg := (quotientReg(0, config.width bits) ## !addOrSub).asSInt
    dividendReg := (!adder.fullSum.msb ## adder.fullSum(0, config.width bits) ## dividendReg(0, config.width - 1 bits) ## B(0, 1 bits)).asSInt
    counter.increment()
  }.elsewhen(counter === U(config.width)) {
    // adjust quotient and remainder when the last partial remainder's sign isn't equal to dividend's sign
    when(dividendReg.msb =/= io.dividend.msb) {
      val adderForRemainder, adderForQuotient = BasicAdder(config.width + 1, AdderType.RCA).setDefinitionName("adderForResult")
      adderForRemainder.setName("adderForRemainder")
      adderForQuotient.setName("adderForQuotient")

      when(dividendReg.msb === io.divisor.msb) {
        adderForRemainder.x   := dividendReg(config.width, config.width + 1 bits).asUInt
        adderForRemainder.y   := (io.divisor.asBits ^ B(config.width + 1 bits, default -> true)).asUInt
        adderForRemainder.cIn := U(1, 1 bits)

        dividendReg := (adderForRemainder.fullSum(0, config.width + 1 bits).asBits ## dividendReg(0, config.width bits)).asSInt

        adderForQuotient.x   := (!quotientReg(config.width - 1, 1 bits).asBool ## quotientReg(0, config.width - 1 bits).asBits ## B(1, 1 bits)).asUInt
        adderForQuotient.y   := U(1, config.width + 1 bits)
        adderForQuotient.cIn := U(0, 1 bits)

        quotientReg := (adderForQuotient.fullSum(0, config.width + 1 bits)).asSInt

        counter.increment()
      } otherwise {
        adderForRemainder.x   := dividendReg(config.width, config.width + 1 bits).asUInt
        adderForRemainder.y   := io.divisor.asUInt
        adderForRemainder.cIn := U(0, 1 bits)

        dividendReg := (adderForRemainder.fullSum(0, config.width + 1 bits).asBits ## dividendReg(0, config.width bits)).asSInt

        adderForQuotient.x   := (!quotientReg(config.width - 1, 1 bits).asBool ## quotientReg(0, config.width - 1 bits).asBits ## B(1, 1 bits)).asUInt
        adderForQuotient.y   := (U(1, config.width + 1 bits).asBits ^ B(config.width + 1 bits, default -> true)).asUInt
        adderForQuotient.cIn := U(1, 1 bits)

        quotientReg := (adderForQuotient.fullSum(0, config.width + 1 bits)).asSInt

        counter.increment()
      }
    } otherwise {
      dividendReg := dividendReg
      quotientReg := (!quotientReg(config.width - 1, 1 bits).asBool ## quotientReg(0, config.width - 1 bits).asBits ## B(1, 1 bits)).asSInt
      counter.increment()
    }
  }.elsewhen(counter === U(config.width + 1)) {
    when(dividendReg(config.width, config.width + 1 bits).abs === io.divisor.abs) {
      dividendReg := (B(0, config.width + 1 bits) ## dividendReg(0, config.width bits)).asSInt
      when(quotientReg.msb) {
        quotientReg := quotientReg - S(1)
      } otherwise {
        quotientReg := quotientReg + S(1)
      }
      counter.increment()
    } otherwise {
      counter.increment()
    }

  } otherwise {}

}

object TestNonRestoringDivider extends App {
  SimConfig.withWave.allOptimisation
    .compile(new NonRestoringDivider(DividerConfig(true, 4)))
    .doSim { dut =>
      import dut.{clockDomain, io}
      clockDomain.forkStimulus(10)
      clockDomain.assertReset()
      io.dividend #= 53
      io.divisor  #= 7

      clockDomain.deassertReset()
      clockDomain.waitSampling(dut.config.width + 3)
      println(s"the devidend is : ${io.dividend.toInt}")
      println(s"the divisor is : ${io.divisor.toInt}")
      println(s"the quotient is : ${io.quotient.toInt}")
      println(s"the remainder is : ${io.remainder.toInt}")

    }
}
