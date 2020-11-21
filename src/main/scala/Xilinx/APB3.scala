package Xilinx

import spinal.core._
import spinal.lib.{IMasterSlave, slave}

case class APB3Config(
                       addressWidth: Int,
                       dataWidth: Int,
                       selWidth: Int = 1,
                       useSlaveError: Boolean = true
                     )

case class APB3(config: APB3Config) extends Bundle with IMasterSlave {
  val PADDR = UInt(config.addressWidth bit)
  val PSEL = Bits(config.selWidth bits)
  val PENABLE = Bool
  val PREADY = Bool
  val PWRITE = Bool
  val PWDATA = Bits(config.dataWidth bit)
  val PRDATA = Bits(config.dataWidth bit)
  val PSLVERROR = if (config.useSlaveError) Bool else null

  override def asMaster(): Unit = { // 定义master的方向,进而定义了slave方向
    out(PADDR, PSEL, PENABLE, PWRITE, PWDATA)
    in(PREADY, PRDATA)
    if (config.useSlaveError) in(PSLVERROR)
  }
}

class Arbiter extends Component {

  val apb3config = APB3Config(16, 32, 1, false)

  val io = new Bundle {
    val apb = slave(APB3(apb3config)) // master和slave的I/O方向翻转
  }
  io.apb.PREADY := True
  io.apb.PRDATA := io.apb.PWDATA
}

object Arbiter {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Arbiter)
  }
}


