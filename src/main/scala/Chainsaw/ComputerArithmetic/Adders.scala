package Chainsaw.ComputerArithmetic

import Chainsaw.ComputerArithmetic.AdderType.RCA
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real

object AdderType extends Enumeration {
  type AdderType = Value
  val RAW, RCA = Value
}

import AdderType._

case class AdderConfig(bitWidth: Int = 0,
                       adderType: AdderType = RCA,
                       hasCIn: Boolean = true, hasCOut: Boolean = true, signed: Boolean = true,
                       hasOverflow: Boolean = true, doSaturation: Boolean = false) {
  if (adderType == RAW) require(!(hasOverflow && signed && !hasCOut), "raw implementation doesn't support overflow logic for signed")
  if (hasCOut) require(!hasOverflow && !doSaturation, "with carry, there's no overflow, and thus no saturation")
  if (!hasCOut) require(hasOverflow && doSaturation,
    "for clarity in test, we bound saturation with overflow and use them whenever there's a mistake of producing incorrect sum")
  override def toString = s"$bitWidth-bit ${if (signed) "signed " else "unsigned "}adder implemented by $adderType which " +
    Seq(hasCIn, hasCOut, hasOverflow, doSaturation).zip(Seq("has cIn", "has cOut", "has overflow", "does saturation")).filter(_._1).map(_._2).mkString(", ")
}

// the core model of adders, all elements are bits, the signed version are implemented by object/classes that invoke this
case class Adder(config: AdderConfig) extends Component {

  import config._

  val io = new Bundle {
    val x, y = in Bits (bitWidth bits)
    // TODO: consider merging cOut and s as one port
    val s = out Bits (bitWidth bits)
    val cIn = if (hasCIn) in Bool() else null // c_0
    val cOut = if (hasCOut) out Bool() else null // c_k
    val overflow = if (hasOverflow) out Bool() else null
  }

  // connection of the inner part and the outer part, handle the conditional ports
  val coreAdder = basicAdder(bitWidth, adderType)
  coreAdder.x := io.x.asUInt
  coreAdder.y := io.y.asUInt
  if (hasCIn) coreAdder.cIn := io.cIn.asUInt else coreAdder.cIn := U"0"

  val overflow = Bool()
  if (signed && !hasCOut) {
    println("this branch")
    overflow := coreAdder.fullSum.msb ^ coreAdder.cSMS
  }
  else if (!signed && !hasCOut) overflow := coreAdder.fullSum.msb
  else overflow := False // when hasCOut, no overflow
  if (hasOverflow) io.overflow := overflow

  val original = coreAdder.fullSum(bitWidth - 1 downto 0).asBits
  // determine io.s
  if (doSaturation) {
    if (signed) {
      when(io.x.msb && overflow)(io.s := B(BigInt(1) << (bitWidth - 1))) // down
        .elsewhen(!io.x.msb && overflow)(io.s := B((BigInt(1) << (bitWidth - 1)) - 1)) // up
        .otherwise(io.s := original)
    }
    else {
      when(overflow)(io.s := io.s.getAllTrue) // up
        .otherwise(io.s := original)
    }
  } else io.s := original

  // determine cOut
  if (hasCOut) if (signed) io.cOut := (!coreAdder.cSMS & (io.x.msb | io.y.msb)) | (coreAdder.cSMS & io.x.msb & io.y.msb)
  else io.cOut := coreAdder.fullSum.msb
}


/** The unsigned binary adder with carry in and out, it is the core of adder design
 *
 * @param bitWidth
 * @param adderType
 */
case class basicAdder(bitWidth: Int, adderType: AdderType) extends Component {
  val x, y = in UInt (bitWidth bits)
  val cIn = in UInt (1 bits)
  val fullSum = out UInt (bitWidth + 1 bits)
  val cSMS = out Bool() // c_{k-1}

  adderType match {
    case RAW => {
      fullSum := (x +^ y + cIn)
      cSMS := (x(bitWidth - 2 downto 0) +^ y(bitWidth - 2 downto 0) + cIn).msb
    }
    case RCA => {
      val carrys = cIn.asBool +: (0 until bitWidth - 1).map(_ => Bool())
      val FAs = (0 until bitWidth).map(i => FA(x(i), y(i), carrys(i)))
      carrys.tail.zip(FAs.init).foreach { case (carry, fa) => carry := fa.carry }
      fullSum := (FAs.last.carry ## FAs.map(_.sum).asBits()).asUInt
      cSMS := carrys.last
    }
  }
}

object Adder {
  def main(args: Array[String]): Unit = {
    val config = AdderConfig(4, RCA, hasCIn = true, hasCOut = true, signed = true, hasOverflow = false, doSaturation = false)

    def value2C(number: BigInt, width: Int) = {
      if (width == 1) number
      else {
        val binary = number.toString(2).padToLeft(width, '0')
        -binary.head.asDigit * (BigInt(1) << number.bitLength - 1) + BigInt(binary.tail, 2)
      }
    }

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

          // TODO: signed assertion
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

    import scala.util.{Try, Success, Failure}
    // traverse all possible configurations and test
    val TF = Seq(true, false)
    for (adderType <- AdderType.values; hasCIn <- TF; hasCOut <- TF; signed <- TF; hasOverflow <- TF; doSaturation <- TF) {
      val config = Try(AdderConfig(4, adderType, hasCIn, hasCOut, signed, hasOverflow, doSaturation))
      config match {
        case Failure(exception) =>
        case Success(value) => {
          printlnGreen(s"start testing ${value.toString}")
          testAdder(value)
        }
      }
    }
  }
}
