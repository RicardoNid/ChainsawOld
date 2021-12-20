package Chainsaw.DSP

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class Crossbar0() extends Component {

  val dataType = HardType(SInt(16 bits))
  val controlType = HardType(Bits(8 bits))

  val dataIn = in Vec(dataType(), 8)
  val controlIn = in Vec(controlType(), 8)
  val dataOut = out(dataType())
  val choosen = out(controlType())

  val fifos = Seq.tabulate(8, 8)((_, _) => StreamFifo(dataType(), 4))
  Seq.tabulate(8, 8) { (i, j) =>
    fifos(i)(j).io.push.valid := controlIn(i)(j)
    fifos(i)(j).io.push.payload := dataIn(i)
  }

  def getLeadingOne(bits: Bits): Bits = {
    val bools = bits.asBools.reverse
    Seq.tabulate(8) { i =>
      if (i == 0) bools(i)
      else ~bools.take(i).orR && bools(i)
    }.reverse.asBits()
  }

  val rets: Seq[(SInt, Bool, Bits)] = (0 until 8).map { j =>
    val group = fifos.map(_.apply(j))
    val candidates: Seq[SInt] = group.map(_.io.pop.payload)
    val valids: Bits = group.map(_.io.pop.valid).reverse.asBits()
    val choosen = getLeadingOne(valids)
    (PriorityMux(valids, candidates), valids.andR, choosen)
  }

  val candidates: Seq[SInt] = rets.map(_._1)
  val valids = rets.map(_._2).reverse.asBits()
  val choosens = rets.map(_._3)

  val winner = PriorityMux(valids, candidates)
  choosen := getLeadingOne(valids)

  Seq.tabulate(8, 8)((i, j) => fifos(i)(j).io.pop.ready := choosen(j) && choosens(j)(i))
  dataOut := winner
}

case class Crossbar1() extends Component{
  val dataType = HardType(SInt(16 bits))
  val controlType = HardType(Bits(8 bits))

  val dataIn = in Vec(dataType(), 8)
  val controlIn = in Vec(controlType(), 8)
  val dataOut = out(dataType())
  val choosen = out(controlType())

  val bufferRegs = Seq.fill(8)(Reg(Vec(dataType(), 4)))
  val counters = Seq.fill(8)(CounterUpDown(4))


}

object Crossbar {
  def main(args: Array[String]): Unit = {
    VivadoSynth(Crossbar0())
  }
}
