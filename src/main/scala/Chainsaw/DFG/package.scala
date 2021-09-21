package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

package object DFG {

  implicit class implUtil(implv: Seq[Bits] => Bits) {

    def asDSPNode(delayv: Int, executionTimev: Double, width: Int = -1, name: String = "") = {
      new DSPNode {
        override def impl(dataIn: Seq[Bits]): Bits = {
          val ret = implv(dataIn)
          ret.setName(name)
          ret
        }

        override def implWidth: Int = width

        override def delay: Int = delayv

        override def executionTime: Double = executionTimev
      }
    }

  }

}
