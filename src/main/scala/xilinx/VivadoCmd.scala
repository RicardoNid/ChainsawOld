package xilinx

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.sys.process._

object VivadoCmd {

  def exec(command: String) = {
    Process(s"/tools/Xilinx/Vivado/2021.1/bin/vivado -mode tcl")
    println("here")
    Process("pwd")
  }

  def main(args: Array[String]): Unit = {
    VivadoCmd.exec("read_verilog ./top.v")
  }

}
