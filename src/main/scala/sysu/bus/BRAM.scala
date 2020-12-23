package sysu.bus

import spinal.core._
import spinal.lib._


/**
  * Bus BRAM configuration
  * @param dataWidth    : data width
  * @param addressWidth : address width
  */
case class BRAMConfig(dataWidth: Int, addressWidth: Int)


/**
  * Bus BRAM definition
  */
case class BRAM(config: BRAMConfig) extends Bundle with IMasterSlave {

  assert(config.dataWidth > 0 & config.dataWidth  % 8 == 0, "BRAM : dataWidth must be a multiple of 8")

  val en     = Bool
  val we     = Bits(config.dataWidth / 8 bits) // write byte enable
  val addr   = UInt(config.addressWidth bits)
  val wrdata = Bits(config.dataWidth bits)
  val rddata = Bits(config.dataWidth bits)


  /** Set the direction of the bus when it is used as master */
  override def asMaster(): Unit = {
    out(en, we, addr, wrdata)
    in(rddata)
  }

  /**
    * Connect two BRAM bus together Master >> Slave
    */
  def >> (sink: BRAM): Unit = {
    assert(this.config.addressWidth >= sink.config.addressWidth, "BRAM mismatch width address (slave address is bigger than master address )")
    assert(this.config.dataWidth == sink.config.dataWidth, "BRAM mismatch width data (slave and master doesn't have the same data width)")

    this.rddata := sink.rddata

    sink.addr   := this.addr.resized
    sink.we     := this.we
    sink.wrdata := this.wrdata
    sink.en     := this.en
  }

  /** Slave << Master */
  def << (sink: BRAM): Unit = sink >> this
}
