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
                       hasOverflow: Boolean = true) {
  if (adderType == RAW) require(!(hasOverflow && signed && !hasCOut), "raw implementation doesn't support overflow logic for signed")
}

// the core model of adders, all elements are bits, the signed version are implemented by object/classes that invoke this
case class Adder(config: AdderConfig) extends Component {

  import config._

  val io = new Bundle {
    val x, y, s = Bits(bitWidth bits)
    val cIn = if (hasCIn) Bool() else null
    val cOut = if (hasCOut) Bool() else null
    val overFlow = if (hasOverflow) Bool() else null
    in(x, y, cIn)
    out(s, cOut)
  }

  val Seq(x, y, s) = Seq(io.x, io.y, io.s)
  val cIn, cOut = Bool()
  val fullSum = Bits(bitWidth + 1 bits)

  // connection of the inner part and the outer part, handle the conditional ports
  if (hasCIn) cIn := io.cIn else cIn := False
  if (hasCOut) io.cOut := cOut
  io.s := fullSum(bitWidth - 1 downto 0)
  cOut := fullSum.msb

  val coreAdder = CoreAdder(bitWidth, adderType, hasOverflow && signed && !hasCOut)
  coreAdder.x := x.asUInt
  coreAdder.y := y.asUInt
  coreAdder.cIn := cIn.asUInt
  fullSum := coreAdder.fullSum.asBits

  if (hasOverflow) {
    if (signed && !hasCOut) io.overFlow := coreAdder.fullSum.msb ^ coreAdder.cSMS
  }
}

case class CoreAdder(bitWidth: Int, adderType: AdderType, needCSMS: Boolean) extends Component {
  val x, y = in UInt (bitWidth bits)
  val cIn = in UInt (1 bits)
  val fullSum = out UInt (bitWidth + 1 bits)
  val cSMS = if (needCSMS) out Bool() else null

  adderType match {
    case RAW => fullSum := (x +^ y + cIn)
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
    val config = AdderConfig(4, RCA, true, true, signed = false)
    GenRTL(Adder(config))
    SimConfig.withWave.compile(new Adder(config)).doSim { dut =>
      import dut._
      io.x #= BigInt("1001", 2) // -7/9
      io.y #= BigInt("1011", 2) // -5/11
      io.cIn #= true // -1/1
      sleep(1)
      if (dut.config.signed) assert(io.cOut.toBigInt * (-16) + io.s.toBigInt == BigInt(-13))
      else assert(io.cOut.toBigInt * 16 + io.s.toBigInt == BigInt(21))
    }
  }
}
