package Chainsaw.ComputerArithmetic

import Chainsaw._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class AdderTest extends AnyFunSuite {

  def testAdder(config: AdderConfig) = {
    SimConfig.withWave.compile(new Adder(config) {
      val combined = if (config.hasCOut) io.cOut ## io.s else io.s
      val dutFullSum = if (config.signed) combined.asSInt else combined.asUInt
      dutFullSum.simPublic()
    }).doSim { dut =>
      import dut._
      def testOnce() = {
        io.x.randomize()
        io.y.randomize()
        if (dut.config.hasCIn) io.cIn.randomize()

        val valueX = if (dut.config.signed) value2C(io.x.toBigInt, dut.config.bitWidth) else io.x.toBigInt
        val valueY = if (dut.config.signed) value2C(io.y.toBigInt, dut.config.bitWidth) else io.y.toBigInt
        val valueCIn = if (dut.config.hasCIn) io.cIn.toBigInt else BigInt(0)
        val valueDut = dutFullSum.toBigInt
        val valueCorrect = valueX + valueY + valueCIn

        val correct = valueDut == valueCorrect

        if (dut.config.hasOverflow) {
          val overflow = io.overflow.toBoolean
          if (dut.config.doSaturation) {
            val saturated = if (!dut.config.signed) coreAdder.x.maxValue
            else if (valueX + valueY > 0) coreAdder.x.maxValue >> 1
            else -((coreAdder.x.maxValue + 1) / 2)
            val correctSaturation = overflow && (valueDut == saturated)
            assert(correctSaturation ^ correct)
          }
          else assert(overflow ^ correct) // only one of them can be true
        } else assert(correct)
        sleep(1)
      }
      (0 until 100).foreach(_ => testOnce())
    }
  }

  test("testAllConfigurations") {
    import scala.util.{Failure, Success, Try}
    // traverse all possible configurations and test
    val TF = Seq(true, false)
    for (adderType <- AdderType.values; hasCIn <- TF; hasCOut <- TF; signed <- TF; hasOverflow <- TF; doSaturation <- TF) {
      val config = Try(AdderConfig(4, adderType, hasCIn, hasCOut, signed, hasOverflow, doSaturation))
      config match {
        case Failure(exception) => // just drop the impossible configuration
        case Success(value) => {
          printlnGreen(s"start testing ${value.toString}")
          testAdder(value)
        }
      }
    }
  }
}
