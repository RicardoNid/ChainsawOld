package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

object RunFTNMatlab extends App {

  eng.eval(s"cd $FTNMatlabWorkSpace")
  eng.eval("channel = 3:226;")
  eng.eval("run main(2, channel)")

}
