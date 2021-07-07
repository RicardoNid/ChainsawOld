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
  if (hasCOut) require(!hasOverflow && !doSaturation, "when there's cOut, no overflow, and thus no saturation would happen")
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
  val coreAdder = CoreAdder(bitWidth, adderType, hasOverflow && signed && !hasCOut)
  coreAdder.x := io.x.asUInt
  coreAdder.y := io.y.asUInt
  if (hasCIn) coreAdder.cIn := io.cIn.asUInt else coreAdder.cIn := U"0"

  val overflow = Bool()
  if (signed && !hasCOut) overflow := coreAdder.fullSum.msb ^ coreAdder.cSMS
  else if (!signed && !hasCOut) overflow := coreAdder.fullSum.msb
  else overflow := False // when hasCOut, no overflow
  if (hasOverflow) io.overflow := overflow

  val original = coreAdder.fullSum(bitWidth - 1 downto 0).asBits
  if (doSaturation) { // hasCOut must be false
    if (signed) {
      when(io.x.msb && overflow)(io.s := B(BigInt(1) << (bitWidth - 1))) // down
        .elsewhen(!io.x.msb && overflow)(io.s := B(BigInt(1) << (bitWidth - 1) - 1)) // up
        .otherwise(io.s := original)
    } else {
      when(overflow)(io.s := io.s.getAllTrue) // up
        .otherwise(io.s := original)
    }
  } else io.s := original

  if (hasCOut) io.cOut := coreAdder.fullSum.msb
}

case class CoreAdder(bitWidth: Int, adderType: AdderType, needCSMS: Boolean) extends Component {
  val x, y = in UInt (bitWidth bits)
  val cIn = in UInt (1 bits)
  val fullSum = out UInt (bitWidth + 1 bits)
  val cSMS = if (needCSMS) out Bool() else null // c_{k-1}

  adderType match {
    case RAW => fullSum := (x +^ y + cIn)
    case RCA => {
      val carrys = cIn.asBool +: (0 until bitWidth - 1).map(_ => Bool())
      val FAs = (0 until bitWidth).map(i => FA(x(i), y(i), carrys(i)))
      carrys.tail.zip(FAs.init).foreach { case (carry, fa) => carry := fa.carry }
      fullSum := (FAs.last.carry ## FAs.map(_.sum).asBits()).asUInt
      if (needCSMS) cSMS := carrys.last
    }
  }
}

object Adder {
  def main(args: Array[String]): Unit = {
    val config = AdderConfig(4, RCA, hasCIn = true, hasCOut = false, signed = false, hasOverflow = true, doSaturation = true)
    GenRTL(Adder(config))
    SimConfig.withWave.compile(new Adder(config) {
      val combined = if (config.hasCOut) io.cOut ## io.s else io.s
      val trueSum = if (config.signed) combined.asSInt else combined.asUInt
      trueSum.simPublic()
    }).doSim { dut =>
      import dut._
      def testOnce() = {
        io.x.randomize()
        io.y.randomize()
        io.cIn.randomize()
        val overflow = io.overflow.toBoolean
        // TODO: signed assertion
        val correct = trueSum.toBigInt == io.x.toBigInt + io.y.toBigInt + io.cIn.toBigInt
        assert(overflow ^ correct) // only one of them can be true
        sleep(1)
      }
      (0 until 100).foreach(_ => testOnce())
    }
  }
}
