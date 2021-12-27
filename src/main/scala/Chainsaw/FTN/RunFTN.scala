package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object RunFTN  {
  def apply() = {
    eng.eval("cd ~/FTN326")
    eng.eval("main([3:226],1,0,0)")
  }
}
