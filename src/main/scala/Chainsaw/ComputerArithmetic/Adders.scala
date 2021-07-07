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
    val x, y = in Bits (bitWidth bits)
    val s = out Bits (bitWidth bits)
    val cIn = if (hasCIn) in Bool() else null
    val cOut = if (hasCOut) out Bool() else null
    val overFlow = if (hasOverflow) out Bool() else null
  }

  // connection of the inner part and the outer part, handle the conditional ports
  val coreAdder = CoreAdder(bitWidth, adderType, hasOverflow && signed && !hasCOut)
  coreAdder.x := io.x.asUInt
  coreAdder.y := io.y.asUInt
  if (hasCIn) coreAdder.cIn := io.cIn.asUInt else coreAdder.cIn := U"0"
  if (hasCOut) io.cOut := coreAdder.fullSum.msb
  io.s := coreAdder.fullSum(bitWidth - 1 downto 0).asBits

  if (hasOverflow) {
    if (signed && !hasCOut) io.overFlow := coreAdder.fullSum.msb ^ coreAdder.cSMS
    else if (!signed && !hasCOut) io.overFlow := coreAdder.fullSum.msb
    else io.overFlow := False // when hasCOut, no overflow
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
      if (needCSMS) cSMS := carrys.last
    }
  }

}

object Adder {
  def main(args: Array[String]): Unit = {
    val config = AdderConfig(4, RCA, hasCIn = true, hasCOut = false, signed = false)
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
        val overflow = io.overFlow.toBoolean
        val correct = trueSum.toBigInt == io.x.toBigInt + io.y.toBigInt + io.cIn.toBigInt
        assert(overflow ^ correct) // only one of them can be true
        sleep(1)
      }
      (0 until 100).foreach(_ => testOnce())
    }
  }
}
