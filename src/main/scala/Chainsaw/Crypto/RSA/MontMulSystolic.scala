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
  val run = Bool() // the overall control
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
  when(io.controlIn.run) {
    SLower := SumLower(w - 2 downto 0) // w - 1 bits
    SO := SumHigherOdd.lsb.asUInt
    SE := SumHigherEven.lsb.asUInt
    CO := SumHigherOdd(2 downto 1)
    CE := SumHigherEven(2 downto 1)
  }

  // output
  io.dataOut.S0 := SLower.lsb.asUInt
  io.dataOut.SComp := (Mux(io.dataIn.S0.asBool, SO, SE) ## SLower(w - 2 downto 1)).asUInt
  io.dataOut.YWord := RegNext(io.dataIn.YWord)
  io.dataOut.MWord := RegNext(io.dataIn.MWord)
  io.dataOut.SetXi := RegNext(io.dataIn.SetXi)
  io.dataOut.SetXi.init(False)
}


/**
 * @param lN sizes of the MontMul that are supported
 * @param w  word size of the MontMulPE
 * @param p  number of the MontMulPE
 */
case class MontMulSystolic(lN: Int, w: Int, p: Int) extends Component {

  val n = lN + 2
  val e = ceil((n + 1).toDouble / w).toInt // number of words
  val round = ceil(n.toDouble / p).toInt // number of words
  val outputProvider = (n - 1) % p // the index of PE that provides the result(starts from 0)
  require(p <= e, "currently, we would require p <= e as p > e leads to grouped input and thus, more complex input pattern")
  printlnGreen(
    s"\n********systolic array properties report:********" +
      s"\n\tsize of Montgomery Multiplication is lM = $lN" +
      s"\n\tword size w = $w, word number e = $e" +
      s"\n\tnumber of PE p = $p" +
      s"\n\tto get the result, $round rounds of iteration will be executed," +
      s"\n\tand the initiation interval is e * r = ${e * round}" +
      s"\n\tthus, PE utilization is ${n.toDouble / (e * round)}" +
      s"\n\tfirst valid output(word) would be ${n + e - 1} cycles after the first valid input(word)" +
      s"\n\tthis systolic accept ${} inputs as a group, and would finish Montgomery Multiplications in" +
      s"\n\tcurrently, we would require p <= e as p > e leads to grouped input and thus, more complex input pattern" +
      s"\n\tthe protocol is as follow: " +
      s"\n\t\tio.run should be set through the calculation, name the first cycle with io.run set as cycle 0" +
      s"\n\t\tfor cycle i <- 0 to e-1, words of M and Y, M(i) and Y(i) should be provided to io.YWordIn and io.MWordIn" +
      s"\n\t\tfor cycle j <- 0 to p-1, bits of X, X(j) should be provided to io.XiIn, then" +
      s"\n\t\tfor cycle j <- e to e + p - 1, continues from X(j - (e - p)), this continues until X is fully consumed" +
      s"\n********systolic array properties report:********")

  val io = new Bundle {
    val start = in Bool()
    // data input
    val xiIn = in UInt (1 bits)
    val YWordIn = in UInt (w bits)
    val MWordIn = in UInt (w bits)
    val SWordOut = out UInt (w bits)
    val validOut = out Bool()
  }

  val runFlag = RegInit(False)
  val PEs = (0 until p).map(_ => new MontMulPE(w))
  // connecting PEs with each other
  PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.dataIn := prev.io.dataOut }
  // for systolic, a FIFO is not needed, a Delay is enough, and simpler
  //  val Queue = FIFO(MontMulPEData(w), if (p - e > 0) p - e else 0)
  val Queue = MontMulPEPass(w)
  val QueueDepth = if (e - p > 0) e - p else 0

  Queue.SetXi := Delay(PEs.last.io.dataOut.SetXi, QueueDepth, init = False)
  Queue.S0 := Delay(PEs.last.io.dataOut.S0, QueueDepth, init = U(0))
  Queue.SComp := Delay(PEs.last.io.dataOut.SComp, QueueDepth, init = U(0))
  Queue.MWord := Delay(PEs.last.io.dataOut.MWord, QueueDepth, init = U(0))
  Queue.YWord := Delay(PEs.last.io.dataOut.YWord, QueueDepth, init = U(0))

  PEs.head.io.dataIn := Queue
  io.SWordOut := (PEs(outputProvider).io.dataOut.SComp ## PEs(outputProvider).io.dataOut.S0).asUInt

  // PE control inputs
  PEs.zipWithIndex.foreach { case (pe, i) =>
    pe.io.controlIn.run := runFlag
    pe.io.controlIn.xi := io.xiIn
  }

  // TODO: improve this, this looks smart, but would be a bad design when lM(and thus, e) is large
  // 1 for start -> run, n + e - 1 for the whole interval, round * (e - p) for waiting - (e - 1) for where the output word started
  val validStart = Delay(io.start, (n + e - 1) + (round - 1) * (e - p) - e + 1 + 1, init = False)
  val validHistory = History(validStart, e, init = False)
  io.validOut := validHistory.asBits.orR

  // counters
  val eCounter = Counter(e)
  val roundCounter = Counter(round, inc = eCounter.willOverflow)

  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val RUN = State()
    IDLE.whenIsActive {
      when(io.start)(goto(RUN))
    }
    IDLE.onExit {
      runFlag.set()
    }
    RUN.whenIsActive {
      PEs.foreach(_.io.controlIn.run := True)

      when(roundCounter.value === U(0)) {
        // PE data input
        PEs.head.io.dataIn.S0 := U(0)
        PEs.head.io.dataIn.SComp := U(0)
        PEs.head.io.dataIn.MWord := io.MWordIn
        PEs.head.io.dataIn.YWord := io.YWordIn
        PEs.head.io.dataIn.SetXi := False
        when(eCounter.value === U(0))(PEs.head.io.dataIn.SetXi := True)
      }

      eCounter.increment()
      when(roundCounter.willOverflow) {
        when(io.start)(goto(RUN))
          .otherwise(goto(IDLE))
      }
    }
  }
}

object MontMulSystolic {
  def main(args: Array[String]): Unit = {
    //    GenRTL(new MontMulPE(16))
    VivadoSynth(new MontMulPE(32))
  }
}
