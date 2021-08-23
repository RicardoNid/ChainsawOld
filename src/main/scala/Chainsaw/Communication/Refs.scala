package Chainsaw.Communication

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real
import Chainsaw.matlabIO._


object Refs {

  // the default scheme of Matlab is gray
  def qammod(input: Array[Int], bitPerSymbol: Int, gray: Boolean = true): Array[MComplex] = {
    if (bitPerSymbol == 1) Array(new MComplex(-1, 0), new MComplex(1, 0))
    else if (!gray) eng.feval[Array[MComplex]]("qammod", input, Array(1 << bitPerSymbol), "bin")
    else eng.feval[Array[MComplex]]("qammod", input, Array(1 << bitPerSymbol), "gray")
  }
}
