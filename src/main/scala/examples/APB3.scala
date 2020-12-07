// design : 自定义接口设计

package examples

import spinal.core._
import spinal.lib._

case class Apb3Config(
                       addressWidth: Int,
                       dataWidth: Int,
                       selWidth: Int,
                       userSlaveError: Boolean
                     )

case class Apb3 (config: Apb3Config) extends Bundle with IMasterSlave{ // design : 自定义Bundle时,使用case class
  val PADDR = UInt(config.addressWidth bits) // address in byte
  val PSEL = UInt(config.addressWidth bits) // one bit per slave
  val PENABLE = Bool
  val PREADY = Bool
  val PWRITE = Bool
  val PWDATA = Bits(config.dataWidth bits) // write data
  val PRDATA = Bits(config.dataWidth bits) // read data
  val PSLVERROR = if(config.userSlaveError) Bool else null // design : 在Spinal中不是使用Option,而是这样定义可选端口

  override def asMaster(): Unit = { // design : 需要override关键字
    out(PADDR, PSEL, PENABLE, PWRITE, PWDATA)
    in(PREADY, PRDATA)
    if(config.userSlaveError) in(PSLVERROR)
  }

  def writing = PWRITE && PENABLE
}
