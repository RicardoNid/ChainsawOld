package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._


class RegSet extends Component {

  val set = in Bool()
  val dataIn = in UInt(4 bits)
  val dataOut = out UInt(4 bits)

  val order = Seq(0, 2, 4, 6, 1, 3, 5, 7)

  def buildChain(order: Seq[Int], dataIn:UInt)= {
    val reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7 = Reg(UInt(4 bits))
    val regs = Seq(reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7)
    (0 until 8).foreach(i => regs(i).setName(s"reg_$i"))
    val reorderedRegs = (0 until 8).map(i => regs(order(i)))

    when(set)(reg0 := dataIn)
    reorderedRegs.init.zip(reorderedRegs.tail).foreach{ case (prev, next) => next := prev}
    dataOut := reorderedRegs.last
  }
  buildChain(order, dataIn)
}

object RegSet extends App {
  GenRTL(new RegSet)
}
