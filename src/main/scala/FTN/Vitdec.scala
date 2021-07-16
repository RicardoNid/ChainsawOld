package FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm.StateMachine

// the interface follow the Xilinx IP


/** For a (m,n,K) Viterbi decoder whose output rate is n / m and constraint length is K
 *
 * @param input organized as a vector of m Bits, for hard-coded, Bits are only one bit
 * @param config
 */
class Vitdec(config: ConvencConfig, tblen: Int, noTrackBack: Boolean = false) extends Component {

  // TODO: currently, we implement for n = 1, hard-coded only, expand this in the future
  // TODO: leave it as an exercise

  import config._

  val io = new Bundle {
    val frame = in Bits (m bits)
    val decoded = out Bool() // as n = 1
    val selections = out Bits((1 << (K - 1)) bits)  
    val valid = out Bool()
  }

  // BMU
  // TODO: smllest amount of Mem, but high fan-out, may need to be optimized later
  val hammingLUTs = (0 until (1 << m)).map(expected => new HammingFromExpected(expected, m))
  hammingLUTs.foreach(_.input := io.frame)
  val hammings: Seq[Array[UInt]] = expectedOutputs.map(_.map(expected => hammingLUTs(expected).output))

  // ACS
  val metrics: Seq[UInt] = states.map(_ => Reg(UInt(4 bits))) // TODO: find a proper upper bound
  val newMetrics = states.indices.map(i => Array(metrics(i) + hammings(i)(0), metrics(i) + hammings(i)(1))).flatten
  val candMetrics = nextStates.flatten.zip(newMetrics).sortBy(_._1).map(_._2).grouped(2).toArray // construct the connections by sorting
  val selections = candMetrics.map(pair => pair(0) <= pair(1)) // select 0
  val selectedMetrics = selections.zip(candMetrics).map { case (bool, ints) => Mux(bool, ints(0), ints(1)) }
  metrics.zip(selectedMetrics).foreach { case (reg, selected) => reg := selected } // update metrics

  // if you don't want to trace back, the bits of paths need to be saved
  // the main problem is, you need to overwrite many bits in a cycle, which means you need to implement the storage by regs
  if (noTrackBack) {
    val paths = states.map(_ => Reg(Bits(tblen bits)))
    val newPaths = states.indices.map(i => Array(paths(i)(tblen - 2 downto 0) ## B"0", paths(i)(tblen - 2 downto 0) ## B"1")).flatten
    val candPaths = nextStates.flatten.zip(newPaths).sortBy(_._1).map(_._2).grouped(2).toArray // construct the connections by sorting
    val selectedPaths = selections.zip(candPaths).map { case (bool, bits) => Mux(bool, bits(0), bits(1)) }
    paths.zip(selectedPaths).foreach { case (reg, selected) => reg := selected } // update path
    io.decoded := paths(0).msb
  } else {
    
  }

  io.decoded := metrics(0).msb
  io.selections := selections.toSeq.asBits()

  val docodeCounter = CounterFreeRun(K * 100)
  io.valid := docodeCounter >= tblen

//  val fsm = new StateMachine{
//
//  }
}

class HammingFromExpected(expected: BigInt, width: Int) extends Component {
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

class ViterbiTraceback(config: ConvencConfig, tblen: Int) extends Component {

  import config._

  val currentState = RegInit(Bits(K - 1 bits).getZero) // start from all zero
}


object HammingFromExpected {
  def main(args: Array[String]): Unit = {
    val FTNConvConfig = ConvencConfig(7, Array(171, 133))
    val FTNConvTBlen = FTNConvConfig.K * 6
    GenRTL(new Vitdec(FTNConvConfig, FTNConvTBlen))
    VivadoSynth(new Vitdec(FTNConvConfig, FTNConvTBlen))

    val constLen = 7
    val codeGens = Array(171, 133)
    val tblen = constLen * 6

    val coded = "1110000111010101010110101001111000001010000101010111101101101001100011110011111111101011000010001001000101011100010000100001101010101101110001011101111110000100001111100100101110110101110010000010000011011101111010011000010011111101000000100011110111100011111001101001000100101110011010110000"
    val frames = coded.grouped(2).toSeq.map(BigInt(_, 2))
    SimConfig.withWave.compile(new Vitdec(ConvencConfig(constLen, codeGens), tblen)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()

      frames.foreach { frame =>
        io.frame #= frame
        clockDomain.waitSampling()
      }
    }
  }
}


