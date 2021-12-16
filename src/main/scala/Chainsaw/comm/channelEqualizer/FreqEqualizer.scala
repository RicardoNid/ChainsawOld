package Chainsaw.comm.channelEqualizer

import Chainsaw._
import breeze.numerics.abs
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** channel equalizer, for FTN mapping to Xilinx DSP slices
 *
 * @param golden    golden preamble sequence
 * @param iteration number of cycles we used for division
 */
case class FreqEqualizer(golden: Seq[Int], iteration: Int) extends Component {


}
