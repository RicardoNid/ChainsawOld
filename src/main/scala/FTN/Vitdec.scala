package FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm.StateMachine

// the interface does not follow the Xilinx IP

/** For a (m,n,K) Viterbi decoder whose output rate is n / m and constraint length is K
 *
 * @param input organized as a vector of m Bits, for hard-coded, Bits are only one bit
 * @param config
 */
class Vitdec(config: ConvencConfig, tblen: Int, noTrackBack: Boolean = false, debug: Boolean = false) extends Component {
  // TODO: currently, we implement for n = 1, hard-coded only, support more complex mode in the future
  import config._

  val io = new Bundle {
    val dataIn = slave Stream Fragment(Bits(m bits))
    val dataOut = master Flow Fragment(Bool())
  }

  // BM
  // TODO: smllest amount of Mem, but high fan-out, may need to be optimized later
  val hammingLUTs = (0 until (1 << m)).map(expected => new HammingFromExpected(expected, m))
  hammingLUTs.foreach(_.input := io.dataIn.fragment)
  val hammings = Vec(expectedOutputs.map(_.map(expected => hammingLUTs(expected).output)).flatten)

  // ACS, take hammings from BM and make selections
  val metricWidth = 5
  def Metric = UInt(metricWidth bits)
  val metrics: Seq[UInt] = states.map(_ => Reg(Metric)) // TODO: find a proper upper bound

  val newMetrics = states.map(_ => Array.fill(2)(Metric)).flatten // TODO: merge
  newMetrics.indices.foreach(i => newMetrics(i) := metrics(i / 2) + hammings(i))

  val candMetrics = nextStates.flatten.zip(newMetrics) // construct the connections by sorting
    .sortBy(_._1).map(_._2).grouped(2).toArray // zip + sortBy + map: sort A by B
  val selections = candMetrics.map(pair => pair(0) > pair(1))
  val selectedMetrics = selections.zip(candMetrics).map { case (bool, ints) => Mux(bool, ints(1), ints(0)) }
  metrics.zip(selectedMetrics).foreach { case (reg, selected) => reg := selected } // update metrics

  val rising = io.dataIn.fire.rise()
  rising.simPublic()

  def SelectionType = Bits(selections.length bits)
  val ramDepth = 4 * tblen
  val selectionRAMA = Mem(SelectionType, ramDepth)
  val selectionRAMB = Mem(SelectionType, ramDepth)
  val addrWA = Counter(ramDepth)
  val addrWB = Counter(ramDepth)
  val addrRA = cloneOf(addrWA.value).setAsReg()
  val addrRB = cloneOf(addrWB.value).setAsReg()

  // control
  val frameCounter = Counter(tblen)
  val stackCounter = Counter(4, inc = frameCounter.willOverflow)

  val trailing = RegInit(False)
  when(io.dataIn.last)(trailing := True)

  val trailingCountDown = Reg(UInt(2 bits)) // TODO: shorten the trailing periods by a better output scheme, thus improve the utilization
  when(io.dataIn.last) {
    when(frameCounter.willOverflow)(trailingCountDown := U(2, 2 bits))
      .otherwise(trailingCountDown := U(3, 2 bits))
  }
  when(frameCounter.willOverflow)(trailingCountDown := trailingCountDown - 1)
  when(trailingCountDown === U(0, 2 bits) && frameCounter.willOverflow)(trailing := False)

  val selectionBits = Mux(trailing, SelectionType.getZero, selections.toSeq.asBits())

  when(io.dataIn.fire || trailing) { // initialization at the first cycle
    // TODO: will this manner lead to redundant adders?
    frameCounter.increment()
    addrWA.increment()
    addrWB.increment()
    when(addrRA === 0)(addrRA := ramDepth - 1)
      .otherwise(addrRA := addrRA - 1)
    when(addrRB === 0)(addrRB := ramDepth - 1)
      .otherwise(addrRB := addrRB - 1)
  }.otherwise { // initialization at the gap
    metrics.head := 0
    metrics.tail.foreach(_ := U(Metric.maxValue / 2).resized)
    frameCounter.clear()
    stackCounter.clear()
    addrWA.clear()
    addrWB.value := (ramDepth - tblen) // regard ramDepth as 0
    addrRA := ramDepth - 1 // 2 for write to read propagation
    addrRB := tblen - 1
  }

  selectionRAMA(addrWA) := selectionBits
  selectionRAMB(addrWB) := selectionBits
  val traceBackInputA = selectionRAMA.readSync(addrRA)
  val traceBackInputB = selectionRAMB.readSync(addrRB)

  // traceback iteration
  val traceBackStart = Delay(frameCounter.willOverflow, 1, init = False)
  val traceBackA = !stackCounter.lsb // mark the leading half of tracing back A
  val traceBackB = stackCounter.lsb

  val traceBackStateA = Reg(UInt(K - 1 bits))
  val previousBitA = traceBackInputA(traceBackStateA)
  when(traceBackA.rise())(traceBackStateA.clearAll()) // traceback start
    .otherwise(traceBackStateA := traceBackStateA(K - 3 downto 0) @@ previousBitA)

  val traceBackStateB = Reg(UInt(K - 1 bits))
  val previousBitB = traceBackInputB(traceBackStateB)
  when(traceBackB.rise())(traceBackStateB.clearAll())
    .otherwise(traceBackStateB := traceBackStateB(K - 3 downto 0) @@ previousBitB)

  // output
  val outputStackA = Stack(Bool(), tblen)
  val outputStackB = Stack(Bool(), tblen)
  when(traceBackA) {
    outputStackB.push(traceBackStateB(K - 3)) // MSB of the next state
    io.dataOut.fragment := outputStackA.pop()
  }.otherwise {
    outputStackA.push(traceBackStateA(K - 3))
    io.dataOut.fragment := outputStackB.pop()
  }

  // if you don't want to trace back, the bits of paths need to be saved
  // the main problem is, you need to overwrite many bits in a cycle, which means you need to implement the storage by regs
  if (noTrackBack) { // register exchange scheme
    val paths = states.map(_ => Reg(Bits(tblen bits)))
    val newPaths = states.indices.map(i => Array(paths(i)(tblen - 2 downto 0) ## B"0", paths(i)(tblen - 2 downto 0) ## B"1")).flatten
    val candPaths = nextStates.flatten.zip(newPaths).sortBy(_._1).map(_._2).grouped(2).toArray // construct the connections by sorting
    val selectedPaths = selections.zip(candPaths).map { case (bool, bits) => Mux(bool, bits(0), bits(1)) }
    paths.zip(selectedPaths).foreach { case (reg, selected) => reg := selected } // update path
    io.dataOut.fragment := paths(0).msb
  } else {

  }

  val debugs = (debug) generate new Area {
    val selectionsOut = out(SelectionType)
    selectionsOut := selectionBits
    val tblenMet = out Bool()
    tblenMet := frameCounter.willOverflow
  }

  io.dataOut.last := Delay(io.dataIn.last, ramDepth, init = False)
  val validReg = RegInit(False)
  when(stackCounter.willOverflow)(validReg.set())
  when(io.dataOut.last)(validReg.clear()) // TODO: find a better way

  io.dataOut.valid := validReg
  io.dataIn.ready := !trailing
  LatencyAnalysis
}

class HammingFromExpected(expected: BigInt, width: Int) extends Component {
  val input = in Bits (width bits)
  val contents = (0 until (1 << width)).map(i => (i ^ expected).toString(2).filter(_ == '1').size)
  contents.indices.foreach { i =>
    println(s"expected: ${expected.toString(2).padToLeft(width, '0')}, " +
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
    //    GenRTL(new Vitdec(FTNConvConfig, FTNConvTBlen))
    VivadoSynth(new Vitdec(FTNConvConfig, FTNConvTBlen, debug = false))

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
        io.dataIn.fragment #= frame
        clockDomain.waitSampling()
      }
    }
  }
}


