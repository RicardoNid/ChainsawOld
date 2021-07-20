package Chainsaw.Memories

import spinal.core._
import spinal.lib.slave

case class BlockRamConfig(

                         )

/** General BRAM model for FPGAs
 * BRAMs are natively synchronous both at read and write port
 */
case class BlockRam(dataWidth: Int, addressWidth: Int) extends Component {

  val ioA = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))
  val ioB = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))

  val bram = Mem(Bits(dataWidth bits), BigInt(1) << addressWidth)

  ioA.dataOut := bram.readWriteSync(ioA.addr, ioA.dataIn, ioA.en, ioA.we)
  ioB.dataOut := bram.readWriteSync(ioB.addr, ioB.dataIn, ioB.en, ioB.we)

  def preAssign() = {
    ioA.preAssign()
    ioB.preAssign()
  }
}

object XILINX_BRAM18E2 {
  def apply() = BlockRam(18, 10)
}

object XILINX_BRAM18E2_PREASSIGNED {
  def apply() = {
    val ret = XILINX_BRAM18E2()
    ret.preAssign()
    ret
  }
}