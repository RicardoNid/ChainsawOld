package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import matlabIO._

import scala.collection.immutable

// the interface follow the Xilinx IP


/** For a (m,n,K) Viterbi decoder whose output rate is n / m and constraint length is K
 *
 * @param input organized as a vector of m Bits, for hard-coded, Bits are only one bit
 * @param config
 */
class Vitdec(config: ConvencConfig, tblen: Int) extends Component {

  // TODO: currently, we implement for n = 1, hard-coded only, expand this in the future
  // TODO: leave it as an exercise

  import config._

  val inputWidth = codeGens.size

  val io = new Bundle {
    val frame = in Bits (inputWidth bits)
    val decoded = out Bool() // as n = 1
    val valid = out Bool()
  }

  val decimalCodeGens = codeGens.map(value => BigInt(value.toString, 8).toInt) // original codeGens are octal
  val states = 0 until (1 << (constLen - 1))
  def branch(state: Int): Array[Int] = Array(state, state + (1 << (constLen - 1))) // two branch from a current state, add 1/0 to the left
  val branchs = states.map(branch)
  val nextStates0 = states.map(state => state >> 1)
  println(nextStates0.mkString(" "))
  val nextStates1 = states.map(state => (state + (1 << (constLen - 1))) >> 1)
  println(nextStates1.mkString(" "))
  def encode(input: Int): Int = decimalCodeGens // convenc, consume K bits and produce n bits coded
    .map(gen => BigInt(gen & input) // code gen as selector
      .toBinary.map(_.asDigit).reduce(_ ^ _)) // xorR
    .reverse.zipWithIndex.map { case (i, exp) => i * (1 << exp) }.sum // bin2decimal
  val outputs = branchs.map(_.map(encode))

  // BMU
  // TODO: smllest amount of Mem, but high fan-out, may need to be optimized later
  val hammingLUTs = (0 until (1 << inputWidth)).map(expected => new HammingFromConstant(expected, inputWidth))
  hammingLUTs.foreach(_.input := io.frame)
  val hammings: Seq[Array[UInt]] = outputs.map(_.map(expected => hammingLUTs(expected).output))

  // ACS
  val metrics: Seq[UInt] = states.map(_ => Reg(UInt(8 bits)))
  val paths = states.map(_ => Reg(Bits(tblen bits)))

  val newPaths = states.indices.map(i => Array(paths(i)(tblen - 2 downto 0) ## B"0", paths(i)(tblen - 2 downto 0) ## B"1")).flatten
  val newMetrics = states.indices.map(i => Array(metrics(i) + hammings(i)(0), metrics(i) + hammings(i)(1))).flatten
  val nextStates = states.indices.map(i => Array(states(i) >> 1, (states(i) + (1 << (constLen - 1))) >> 1)).flatten

  val cands = nextStates.zip(newMetrics.zip(newPaths)).sortBy(_._1).map(_._2).grouped(2).toArray // construct the connections by sorting
  val candMetrics = cands.map(_.map(_._1))
  val candPaths = cands.map(_.map(_._2))

  val conds = candMetrics.map(pair => pair(0) <= pair(1)) // select 0
  val selectedMetrics = conds.zip(candMetrics).map { case (bool, ints) => Mux(bool, ints(0), ints(1)) }
  val selectedPaths = conds.zip(candPaths).map { case (bool, bits) => Mux(bool, bits(0), bits(1)) }

  metrics.zip(selectedMetrics).foreach { case (reg, selected) => reg := selected } // update metrics
  paths.zip(selectedPaths).foreach { case (reg, selected) => reg := selected } // update path

  io.decoded := paths(0).msb
  val docodeCounter = CounterFreeRun(constLen * 100)
  io.valid := docodeCounter >= tblen
}

class HammingFromConstant(expected: BigInt, width: Int) extends Component {
  val input = in Bits (width bits)
  val contents = (0 until (1 << width)).map(i => (i ^ expected).toBinary.filter(_ == '1').size)
  contents.indices.foreach { i =>
    println(s"expected: ${expected.toBinary.padToLeft(width, '0')}, " +
      s"received: ${i.toBinaryString.padToLeft(width, '0')}, " +
      s"hamming: ${contents(i)}")
  }
  val LUT = Mem(contents.map(U(_, log2Up(width + 1) bits)))
  val output = out(LUT(input.asUInt))
}

object HammingFromConstant {
  def main(args: Array[String]): Unit = {
    val FTNConvConfig = ConvencConfig(7, Array(171, 133))
    val FTNConvTBlen = FTNConvConfig.constLen * 6
    GenRTL(new Vitdec(FTNConvConfig, FTNConvTBlen))
    VivadoSynth(new Vitdec(FTNConvConfig, FTNConvTBlen))
  }
}


