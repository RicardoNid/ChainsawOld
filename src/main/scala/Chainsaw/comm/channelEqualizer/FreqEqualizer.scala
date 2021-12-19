package Chainsaw.comm.channelEqualizer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import spinal.core._

import scala.language.postfixOps

/** channel equalizer, for FTN mapping to Xilinx DSP slices
 *
 * @param golden    golden preamble sequence
 * @param iteration number of cycles we used for division
 */
case class FreqEqualizer(dspType: HardType[SFix], golden: Seq[Int], vecSize:Int) extends Component {

  val dataIn = slave Flow Vec(dspType(), vecSize)
  val dataOut = master Flow Vec(dspType(), vecSize)

  val complexType = HardType(ComplexNumber(dspType))
  val vecType = HardType(Vec(dspType(), vecSize))
  val complexVecType = HardType(Vec(complexType(), vecSize))

  val counter = CounterFreeRun(18)

//  when(counter < U(2))

  val preambleFIFO = StreamFifo(complexVecType(), 10)
  val dataFIFO = StreamFifo(complexVecType(), 80)

  val smoothModule = Smooth(golden, dspType(), vecSize)
  val startSmooth = preambleFIFO.io.occupancy >= U(2)
  smoothModule.dataIn.valid := startSmooth
  smoothModule.dataIn.payload := preambleFIFO.io.pop.payload




}
