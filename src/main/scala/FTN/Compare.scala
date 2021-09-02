package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

object Compare extends App {

  eng.eval("cd ./matlabWorkspace/FTN326")
  eng.eval("channel = 3:226;")
  eng.eval("run main(2, channel)")
  eng.eval("load bitsAllFrame")
  eng.eval("load codedBitsAllFrame")
  val bits = eng.getVariable[Array[Double]]("bitsAllFrame")
  val debits = eng.getVariable[Array[Double]]("debitsAllFrame")
  println(bits.mkString(" "))
  println(debits.mkString(" "))

}
