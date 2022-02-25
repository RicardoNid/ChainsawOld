package Chainsaw.comm.qam

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.language.postfixOps

/** configuration of adaptive Qammod/demod with bit & power allocation
 *
 * @param bitCandidates all valid bit numbers
 * @param basicBitAlloc basic bit allocation situation, providing size and average bit number
 * @example for 16 bits -> 4 symbols, basicBitAlloc should be Seq(4,4,4,4)
 * @param powAllow power allocation
 * @param dataType define the range and precision of normalized symbols
 */
case class DFXQamConfig(bitCandidates: Seq[Int], basicBitAlloc: Seq[Int], powAllow: Seq[Double], dataType: HardType[SFix]) {
  val numSymbols = basicBitAlloc.size
  val lenFrame = basicBitAlloc.sum
  val lenSymbol = lenFrame / numSymbols
}

case class DFXQammod(config: DFXQamConfig) {


}

object DFXQammod extends App {
  val config = DFXQamConfig(Seq(2, 4, 6, 8), Seq(4, 4, 4, 4), Seq(1.0, 1.0, 1.0, 1.0), SFix(2 exp, -15 exp))
  val alloc0 = Seq(2, 2, 4, 8)
  VivadoImpl(BitAllocator(config, alloc0))
  VivadoImpl(BitCombinator(config, alloc0))
}

case class BitAllocator(config: DFXQamConfig, bitAlloc: Seq[Int]) extends Component {

  val dataIn = in Bits (config.lenFrame bits)
  val dataOut = out Vec(Bits(config.bitCandidates.max bits), config.numSymbols)

  val addrs = bitAlloc.scanLeft(0)(_ + _)
  val slices = addrs.init.zip(addrs.tail).map { case (a, b) => config.lenFrame - 1 - a downto config.lenFrame - b }
  dataOut.zip(slices).foreach { case (out, slice) => out := RegNext(dataIn(slice)).resized }

}

case class BitCombinator(config: DFXQamConfig, bitAlloc: Seq[Int]) extends Component {

  val dataIn = in Vec(Bits(config.bitCandidates.max bits), config.numSymbols)
  val dataOut = out Bits (config.lenFrame bits)

  val addrs = bitAlloc.scanLeft(0)(_ + _)
  val slices = addrs.init.zip(addrs.tail).map { case (a, b) => config.lenFrame - 1 - a downto config.lenFrame - b }
  dataIn.zip(slices).foreach { case (in, slice) => dataOut(slice) := RegNext(in).resized }

}