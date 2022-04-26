package Chainsaw.crypto.ReedSolomon

import spinal.core._
import spinal.lib._
import GaloisFieldHardWare._

case class HornerPloy(i: Int, n: Int, k: Int, r: Flow[UInt], reload: Bool) extends ImplicitArea[UInt] {
  val m = n - k

  val gf  = GaloisField(m)
  val reg = Reg(UInt(m bits)) init (0)
  val mul = gf.alphaPowTable(i)

  when(reload) {
    when(r.valid)(reg := r.payload) otherwise (reg.clearAll())
  } elsewhen (r.valid) {
    reg := reg.constMulti(mul).add(r.payload)
  }

  override def implicitValue: UInt = UInt(m bits)
}
