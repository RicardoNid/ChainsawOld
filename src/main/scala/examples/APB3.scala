

package examples

import spinal.core._
import spinal.lib._

case class Apb3Config(
                       addressWidth: Int,
                       dataWidth: Int,
                       selWidth: Int,
                       userSlaveError: Boolean
                     )

case class Apb3 (config: Apb3Config) extends Bundle with IMasterSlave{
  val PADDR = UInt(config.addressWidth bits) // address in byte
  val PSEL = UInt(config.addressWidth bits) // one bit per slave
  val PENABLE = Bool
  val PREADY = Bool
  val PWRITE = Bool
  val PWDATA = Bits(config.dataWidth bits) // write data
  val PRDATA = Bits(config.dataWidth bits) // read data
  val PSLVERROR = if(config.userSlaveError) Bool else null

  override def asMaster(): Unit = {
    out(PADDR, PSEL, PENABLE, PWRITE, PWDATA)
    in(PREADY, PRDATA)
    if(config.userSlaveError) in(PSLVERROR)
  }

  def writing = PWRITE && PENABLE
}
