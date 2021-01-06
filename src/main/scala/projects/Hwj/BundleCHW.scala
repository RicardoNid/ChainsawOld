package projects.Hwj

import spinal.core._
import spinal.lib._
import sysu.CNN._

case class BundleCHW(conv: ToeplitzConv, parallel: Int) extends Bundle with IMasterSlave {
  val channel = UInt(log2Up(conv.Nif) bits)
  val row = UInt(log2Up(conv.Niy) bits)
  val column = UInt(log2Up(conv.Nix) bits)
  val data = Vec(UInt(dataWidth bits), parallel)

  override def asMaster() = {
    out(channel, row, column)
  }
}
