package Chainsaw.comparith

import Chainsaw.comparith.AdderType._
import spinal.core._
import spinal.lib._

/** The unsigned binary adder with carry in and out, it is the core of adder design
 *
 * @param bitWidth
 * @param adderType
 */
case class BasicAdder(bitWidth: Int, adderType: AdderType) extends Component {
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
