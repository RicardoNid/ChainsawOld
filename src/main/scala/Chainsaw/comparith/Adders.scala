package Chainsaw.comparith

import Chainsaw.comparith.AdderType.RCA
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
  val coreAdder = BasicAdder(bitWidth, adderType)
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

