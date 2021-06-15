package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import scala.math.ceil

// TODO: improve the type consistency

case class MontMulPEPass(w: Int) extends Bundle {
  // SComp ## S0 combines a SWord
  val SetXi = Bool()
  val S0 = UInt(1 bits) // 0
  val SComp = UInt(w - 1 bits) // w-1 downto 1
  val YWord = UInt(w bits)
  val MWord = UInt(w bits)
}

case class MontMulPEOther() extends Bundle {
  // xi should be externally registered
  // short as it is, it is a UInt as it has numeric semantic
  val xi = UInt(1 bits)
}

/**
 * @param w the word size of MontMul
 */
class MontMulPE(w: Int) extends Component { // we want it to be synthesized independently

  val io = new Bundle {
    val dataIn = in(MontMulPEPass(w))
    val dataOut = out(MontMulPEPass(w))
    val controlIn = in(MontMulPEOther())
  }

  // data registers
  val CO, CE = RegInit(U(0, 2 bits))
  val SO, SE = RegInit(U(0, 1 bits))
  val SLower = RegInit(U(0, w - 1 bits))
  // long-term data registers
  val qi = RegInit(False)
  val qiInUse = Bool()
  val xi = RegInit(U(0, 1 bits))
  val xiInUse = UInt(1 bits)
  when(io.dataIn.SetXi) { // first cycle of e cycles
    xiInUse := io.controlIn.xi
    xi := io.controlIn.xi
    qiInUse := ((io.dataIn.YWord.lsb & xiInUse.asBool) ^ io.dataIn.SComp.lsb)
    qi := ((io.dataIn.YWord.lsb & xiInUse.asBool) ^ io.dataIn.SComp.lsb) // not S0, S1
  }.otherwise {
    xiInUse := xi
    qiInUse := qi
  }

  // intermediate signals
  val xiYWord = Mux(xiInUse.asBool, io.dataIn.YWord, U(0))
  val qiMWord = Mux(qiInUse, io.dataIn.MWord, U(0))
  val C = UInt(2 bits)
  when(io.dataIn.SetXi)(C := U(0))
    .otherwise(C := Mux(io.dataIn.S0.asBool, CO, CE))

  // w + 1 bits, the lower w - 1 bits are final
  val SumLower = (xiYWord(w - 2 downto 0) +^ C) +^
    (qiMWord(w - 2 downto 0) +^ io.dataIn.SComp)

  val HigherCommonPart = xiYWord.msb.asUInt +^ qiMWord.msb.asUInt // 2 bits
  val SumHigherOdd = (U(1, 1 bits) +^ SumLower(w downto w - 1)) +^ HigherCommonPart // 3 bits
  val SumHigherEven = SumLower(w downto w - 1) +^ HigherCommonPart
  // TODO: for sim
  val SWordRet = io.dataOut.SComp ## io.dataOut.S0
  SWordRet.simPublic()

  // TODO: should the computation be controlled by run? would that be more energy-efficient?
  // update
  SLower := SumLower(w - 2 downto 0) // w - 1 bits
  SO := SumHigherOdd.lsb.asUInt
  SE := SumHigherEven.lsb.asUInt
  CO := SumHigherOdd(2 downto 1)
  CE := SumHigherEven(2 downto 1)

  // output
  io.dataOut.S0 := SLower.lsb.asUInt
  io.dataOut.SComp := (Mux(io.dataIn.S0.asBool, SO, SE) ## SLower(w - 2 downto 1)).asUInt
  io.dataOut.YWord := RegNext(io.dataIn.YWord)
  io.dataOut.MWord := RegNext(io.dataIn.MWord)
  io.dataOut.SetXi := RegNext(io.dataIn.SetXi)
  io.dataOut.SetXi.init(False)
}


/**
 * @param lNs sizes of the MontMul that are supported
 * @param w   word size of the MontMulPE
 * @param p   number of the MontMulPE
 */
case class MontMulSystolic(lNs: Seq[Int], w: Int, p: Int) extends Component {

  // design parameters
  val ns = lNs.map(_ + 2) // total numbers of iterations
  val es = ns.map(n => ceil((n + 1).toDouble / w).toInt) // numbers of words
  val rounds = ns.map(n => ceil(n.toDouble / p).toInt) // numbers of rounds to be executed
  // n + e - 1 for the whole interval, (round - 1) * (e - p) for waiting - (e - 1) for where the output word started, 1 for start -> run
  val latencies = (0 until ns.size).map(i => (ns(i) + es(i) - 1) + (rounds(i) - 1) * (es(i) - p) - es(i) + 1 + 1)
  val outputProviders = ns.map(n => (n - 1) % p) // the index of PE that provides the result(starts from 0)
  val QueueDepths = es.map(e => if (e - p > 0) e - p else 0)
  require(p <= es.min, "currently, we would require p <= e as p > e leads to grouped input and thus, more complex input pattern")
  printlnGreen(
    s"\n********systolic array properties report:********" +
      s"\n\tsupported sizes of Montgomery Multiplications are lM = ${lNs.mkString(" ")} " +
      s"\n\tword size w = $w, word number e = ${es.mkString(" ")}" +
      s"\n\tnumber of PE p = $p" +
      s"\n\tto get the result, ${rounds.mkString(" ")} rounds of iteration will be executed," +
      s"\n\tand the initiation interval is e * r = ${es.zip(rounds).map { case (i, i1) => i * i1 }.mkString(" ")}" +
      s"\n\tthus, PE utilization is ${(0 until ns.size).map(i => ns(i).toDouble / (es(i) * rounds(i))).mkString(" ")}" +
      s"\n\tfirst valid output(word) would be ${latencies.map(_ - 1).mkString(" ")} cycles after the first valid input(word)" +
      s"\n\tthis systolic accept ${} inputs as a group, and would finish Montgomery Multiplications in" +
      s"\n\tcurrently, we would require p <= e as p > e leads to grouped input and thus, more complex input pattern" +
      s"\n\tthe protocol is as follow: " +
      s"\n\t\tio.run should be set through the calculation, name the first cycle with io.run set as cycle 0" +
      s"\n\t\tfor cycle i <- 0 to e-1, words of M and Y, M(i) and Y(i) should be provided to io.YWordIn and io.MWordIn" +
      s"\n\t\tfor cycle j <- 0 to p-1, bits of X, X(j) should be provided to io.XiIn, then" +
      s"\n\t\tfor cycle j <- e to e + p - 1, continues from X(j - (e - p)), this continues until X is fully consumed" +
      s"\n********systolic array properties report:********")

  val io = new Bundle {
    // control
    val start = in Bool()
    val mode = in Bits (lNs.size bits) // one-hot
    // data
    val xiIn = in UInt (1 bits)
    val YWordIn = in UInt (w bits)
    val MWordIn = in UInt (w bits)
    val dataOut = out UInt (w bits)
    val valid = out Bool()
  }

  val PEs = (0 until p).map(_ => new MontMulPE(w))
  // connecting PEs with each other
  PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.dataIn := prev.io.dataOut }
  // for systolic, a FIFO is not needed, a Delay is enough, and simpler
  //  val Queue = FIFO(MontMulPEData(w), if (p - e > 0) p - e else 0)
  val bufferSetXi = History(PEs.last.io.dataOut.SetXi, QueueDepths.max + 1, init = False)
  val bufferS0 = History(PEs.last.io.dataOut.S0, QueueDepths.max + 1, init = U(0, 1 bits))
  val bufferSComp = History(PEs.last.io.dataOut.SComp, QueueDepths.max + 1, init = U(0, w - 1 bits))
  val bufferYWord = History(PEs.last.io.dataOut.YWord, QueueDepths.max + 1, init = U(0, w bits))
  val bufferMWord = History(PEs.last.io.dataOut.MWord, QueueDepths.max + 1, init = U(0, w bits))

  val queues = Vec(QueueDepths.map(_ => MontMulPEPass(w)))
  queues.zip(QueueDepths).foreach { case (queue, depth) =>
    queue.SetXi := bufferSetXi(depth)
    queue.S0 := bufferS0(depth)
    queue.SComp := bufferSComp(depth)
    queue.YWord := bufferYWord(depth)
    queue.MWord := bufferMWord(depth)
  }

  // datapaths
  PEs.head.io.dataIn := MuxOH(io.mode, queues)
  val outputCandidates = outputProviders.map(outputProvider => (PEs(outputProvider).io.dataOut.SComp ## PEs(outputProvider).io.dataOut.S0).asUInt)
  io.dataOut := MuxOH(io.mode, outputCandidates)

  // PE control inputs
  PEs.zipWithIndex.foreach { case (pe, i) =>
    pe.io.controlIn.xi := io.xiIn
  }

  // TODO: implement this
  io.valid := False

  // counters
  val eCounter = Counter(es.max)
  val eCounterWillOverflows = es.map(e => eCounter.value === U(e - 1) && eCounter.willIncrement)
  val currentECounterOverflow = MuxOH(io.mode, eCounterWillOverflows)
  when(currentECounterOverflow)(eCounter.clear())
  val roundCounter = Counter(rounds.max, inc = MuxOH(io.mode, eCounterWillOverflows))

  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val RUN = State()

    IDLE.whenIsActive {
      when(io.start)(goto(RUN))
    }

    RUN.whenIsActive { // control the dataflow
      eCounter.increment()
      when(roundCounter.value === U(0)) {
        // PE data input
        PEs.head.io.dataIn.S0 := U(0)
        PEs.head.io.dataIn.SComp := U(0)
        PEs.head.io.dataIn.MWord := io.MWordIn
        PEs.head.io.dataIn.YWord := io.YWordIn
        PEs.head.io.dataIn.SetXi := False
        when(eCounter.value === U(0))(PEs.head.io.dataIn.SetXi := True)
      }
      when(roundCounter === MuxOH(io.mode, rounds.map(round => U(round - 1))) && currentECounterOverflow) {
        when(io.start)(goto(RUN))
          .otherwise(goto(IDLE))
      }
    }
    RUN.onExit(roundCounter.clear())
  }
}

object MontMulSystolic {
  def main(args: Array[String]): Unit = {
    //    GenRTL(new MontMulPE(16))
    VivadoSynth(new MontMulPE(32))
  }
}
