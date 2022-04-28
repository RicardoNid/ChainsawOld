package Chainsaw.crypto.ReedSolomon

import spinal.core._
import spinal.lib._

case class acc(data: UInt, i: Int, m: Int, calculating: Bool, valid: Bool) extends ImplicitArea[UInt] {
  override def implicitValue: UInt = UInt(m bits)
  import GaloisFieldHardWare._
  val gfm    = GaloisField(m)
  val alphaI = gfm.alphaPowTable(i)
  val accReg = Reg(UInt(m bits)) init (0)
  when(calculating)(accReg := accReg.constMulti(alphaI)) elsewhen (valid)(accReg := data.constMulti(alphaI))
}
