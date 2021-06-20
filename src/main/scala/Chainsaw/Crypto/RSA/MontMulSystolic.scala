package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.math.{ceil, floor}

/**
 * @param lMs sizes of the MontMul that are supported
 * @param w   word size of the MontMulPE
 * @param p   number of the MontMulPE
 */
case class MontConfig(lMs: Seq[Int], w: Int, pe: Int, parallel: Boolean = false) {

  if (parallel) require(lMs.forall(_ % lMs.min == 0), s"for parallel architecture, supported sizes must be multiples of the minimum size")
  val parallelFactor = lMs.max / lMs.min
  val p = if (parallel) floor((lMs.max + 2 + 1).toDouble / w).toInt else pe // when parallel, p = e - 1

  val ns = lMs.map(_ + 2) // total numbers of iterations, r = 2^(n) > 4M
  val es = ns.map(n => ceil((n + 1).toDouble / w).toInt) // numbers of words
  val rounds = ns.map(n => ceil(n.toDouble / p).toInt) // numbers of rounds to be executed

  // the index of PE that provides the result(starts from 0)
  val outputProviders = ns.map(n => (n - 1) % p)
  // the queue depths needed to connect the data of previous and current round
  // these depths indicate the delays, which will be used in the design
  val queueDepths = es.map(e => if (e - p > 0) e - p else 0)
  require(p <= es.min, "currently, we would require p <= e as p > e leads to grouped input and thus, more complex input pattern")
  // data
  val IIs = es.zip(rounds).map { case (i, i1) => i * i1 } // initiation intervals
  val utilizations = (0 until ns.size).map(i => ns(i).toDouble / (p * rounds(i))) // utilizations, tasks / (PE * round)
  val queueUtilizations = (0 until es.size).map(i => (es(i).toDouble - p) / (es.max - p)) // utilizations, tasks / (PE * round)
  // n + e - 1 for the whole interval, (round - 1) * (e - p) for waiting - (e - 1) for where the output word started, 1 for start -> run
  val latencies = (0 until ns.size).map(i => (ns(i) + es(i) - 1) + (rounds(i) - 1) * (es(i) - p) - es(i) + 1 + 1)

  printlnGreen(
    s"\n********systolic array properties report********" +
      s"\n\ta instance of MontMul has been initialized, supported sizes of Montgomery Multiplications are lM = ${lMs.mkString(" ")} " +
      s"\n********AREA/TIMING********" +
      s"\n\tword size w = $w, " +
      s"\n\tnumber of PE p = $p" +
      s"\n\tdepth of queue = ${es.max - p}" +
      s"\n\testimated area is , for Xilinx UltraScale, fMax >=" +
      s"\n\testimated area is , for TSMC 40nm technology, fMax >=" +
      s"\n********AREA/TIMING********" +
      s"\n********WORKING STATUS********" +
      s"\n\tlMs:                     ${lMs.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tword number e:           ${es.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\trounds r:                ${rounds.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tinitiation intervals II: ${IIs.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tlatenciese:              ${latencies.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tPE utilization:          ${utilizations.map(_.toString.padTo(10, ' ').take(8)).mkString("  ")}" +
      s"\n\tqueue utilization:       ${queueUtilizations.map(_.toString.padTo(10, ' ').take(8)).mkString("  ")}" +
      s"\n********WORKING STATUS********" +
      s"\n\tcurrently, we would require p <= e as p > e leads to interleaved input and thus, more complex input pattern" +
      s"\n\tthis systolic accept 1 task as a group, and would finish Montgomery Multiplications in" +
      s"\n\tthe input scheme is as follow: " +
      s"\n\t\tio.run should be set through the calculation, name the first cycle with io.run set as cycle 0" +
      s"\n\t\tfor cycle i <- 0 to e-1, words of M and Y, M(i) and Y(i) should be provided to io.YWordIn and io.MWordIn" +
      s"\n\t\tfor cycle j <- 0 to p-1, bits of X, X(j) should be provided to io.XiIn, then" +
      s"\n\t\tfor cycle j <- e to e + p - 1, continues from X(j - (e - p)), this continues until X is fully consumed" +
      s"\n\tthe output is indicated by valid" +
      s"\n********systolic array properties report********")
}

case class MontMulSystolic(config: MontConfig) extends Component {

  import config._

  val io = new Bundle {
    // control
    val start = in Bool()
    val mode = in Bits (lMs.size bits) // one-hot
    // data
    val xiIn = in UInt (1 bits)
    val YWordIn = in UInt (w bits)
    val MWordIn = in UInt (w bits)

    val flowOut = out UInt (w bits)
    val valid = out Bool()
    val idle = out Bool()
  }

  val modeReg = Reg(HardType(io.mode))
  when(io.start)(modeReg := io.mode)

  // TODO: clean up this part
  val PEs = (0 until p).map(_ => new MontMulPE(w))
  // connecting PEs with each other
  PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.flowIn := prev.io.flowOut }
  // for systolic, a FIFO is not needed, a Delay is enough, and simpler
  //  val Queue = FIFO(MontMulPEData(w), if (p - e > 0) p - e else 0)
  val bufferSetXi = History(PEs.last.io.flowOut.SetXi, queueDepths.max + 1, init = False)
  val bufferS0 = History(PEs.last.io.flowOut.S0, queueDepths.max + 1, init = U(0, 1 bits))
  val bufferSComp = History(PEs.last.io.flowOut.SComp, queueDepths.max + 1, init = U(0, w - 1 bits))
  val bufferYWord = History(PEs.last.io.flowOut.YWord, queueDepths.max + 1, init = U(0, w bits))
  val bufferMWord = History(PEs.last.io.flowOut.MWord, queueDepths.max + 1, init = U(0, w bits))

  val queues = Vec(queueDepths.map(_ => MontMulPEFlow(w)))
  queues.zip(queueDepths).foreach { case (queue, depth) =>
    queue.valid := False
    queue.SetXi := bufferSetXi(depth)
    queue.S0 := bufferS0(depth)
    queue.SComp := bufferSComp(depth)
    queue.YWord := bufferYWord(depth)
    queue.MWord := bufferMWord(depth)
  }

  // datapaths
  PEs.head.io.flowIn := MuxOH(modeReg, queues)
  val outputPEs = outputProviders.map(PEs(_))
  val outputCandidates = outputPEs.map(pe =>
    ((pe.io.flowOut.SComp ## pe.io.flowOut.S0).asUInt, pe.io.flowOut.valid))
  //  val outputCandidates = outputProviders.map(outputProvider =>
  //    (PEs(outputProvider).io.flowOut.SComp ## PEs(outputProvider).io.flowOut.S0).asUInt)
  io.flowOut := MuxOH(modeReg, outputCandidates.map(_._1))
  io.valid := MuxOH(modeReg, outputCandidates.map(_._2))

  // xi input
  PEs.foreach(pe => pe.io.xi := io.xiIn)


  // counters
  val eCounter = Counter(es.max)
  val eCounterWillOverflows = es.map(e => eCounter.value === U(e - 1) && eCounter.willIncrement)
  val currentECounterOverflow = MuxOH(modeReg, eCounterWillOverflows)
  when(currentECounterOverflow)(eCounter.clear())
  val roundCounter = Counter(rounds.max, inc = MuxOH(modeReg, eCounterWillOverflows))

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
        PEs.head.io.flowIn.S0 := U(0)
        PEs.head.io.flowIn.SComp := U(0)
        PEs.head.io.flowIn.MWord := io.MWordIn
        PEs.head.io.flowIn.YWord := io.YWordIn
        PEs.head.io.flowIn.SetXi := False
        when(eCounter.value === U(0))(PEs.head.io.flowIn.SetXi := True)
      }
      when(roundCounter.value === MuxOH(modeReg, rounds.map(round => U(round - 1)))) {
        PEs.head.io.flowIn.valid := True
      }
      when(roundCounter === MuxOH(modeReg, rounds.map(round => U(round - 1))) && currentECounterOverflow) {
        when(io.start)(goto(RUN))
          .otherwise(goto(IDLE))
      }
    }
    RUN.onExit(roundCounter.clear())

    io.idle := isActive(IDLE)
  }
}

object MontMulSystolic {
  def main(args: Array[String]): Unit = {
    //    GenRTL(new MontMulPE(16))
    //    VivadoSynth(new MontMulPE(128))
    VivadoSynth(new MontMulSystolic(MontConfig(Seq(512, 1024, 2048, 3072, 4096), 32, 17)))
  }
}
