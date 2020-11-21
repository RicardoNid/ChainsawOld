package Xilinx

import spinal.core._
import spinal.lib._

class Ram_1w_1r(wordWidth: Int, wordCount: Int) extends BlackBox {

  // SpinalHDL will look at Generic classes to get attributes which
  // should be used as VHDL generics / Verilog parameters
  // You can use String, Int, Double, Boolean, and all SpinalHDL base
  // types as generic values
  val generic = new Generic {
    val wordCount = Ram_1w_1r.this.wordCount
    val wordWidth = Ram_1w_1r.this.wordWidth
  }

  // Define IO of the VHDL entity / Verilog module
  val io = new Bundle {
    val clk = in Bool
    val wr = new Bundle {
      val en   = in Bool
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val en   = in Bool
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  // Map the current clock domain to the io.clk pin
  mapClockDomain(clock=io.clk)
}

// Create the top level and instantiate the Ram
class TopLevel extends Component {
  val io = new Bundle {
    val wr = new Bundle {
      val en = in Bool
      val addr = in UInt (log2Up(16) bit)
      val data = in Bits (8 bit)
    }
    val rd = new Bundle {
      val en = in Bool
      val addr = in UInt (log2Up(16) bit)
      val data = out Bits (8 bit)
    }
  }

  // Instantiate the blackbox
  val ram = new Ram_1w_1r(8, 16)

  // Connect all the signals
  io.wr.en <> ram.io.wr.en
  io.wr.addr <> ram.io.wr.addr
  io.wr.data <> ram.io.wr.data
  io.rd.en <> ram.io.rd.en
  io.rd.addr <> ram.io.rd.addr
  io.rd.data <> ram.io.rd.data
}

object TopLevel {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new TopLevel)
  }
}