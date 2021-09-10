package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

case class PokeExample() extends Component {
  val dataIn = in SInt (8 bits)
  val dataInFixed = in SFix (3 exp,  -4 exp)
  dataInFixed << 3
  val dataOut = out SInt (8 bits)

  dataOut := dataIn
}


object PokeExample extends App {

  implicit class complement (value:Int){
    def as2C = {
      val bits = value.toBinaryString.reverse
      bits.zipWithIndex.map{ case (c, i) => c.asDigit * (1 << i) * (if(i == bits.length - 1) -1 else 1)}.sum
    }
  }

  SimConfig.withWave.compile( PokeExample()).doSim { dut =>
    dut.dataIn #= 144.as2C
    sleep(2)
  }
}

