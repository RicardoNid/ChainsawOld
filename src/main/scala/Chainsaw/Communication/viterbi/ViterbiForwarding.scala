package Chainsaw.Communication.viterbi

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._

// ACS part of Viterbi
case class ViterbiForwarding(trellis: Trellis[Int]) extends Component {

  import trellis._

  val codedBitNum = log2Up(numOutputSymbols)
  val discrepancyWidth = 4
  val discrepancyType = HardType(UInt(discrepancyWidth bits)) // TODO: parameterize the size
  val discrepancyBig = U(1 << (discrepancyWidth - 1), discrepancyWidth bits)

  val incrementWidth = log2Up(codedBitNum + 1)

  val dataIn = slave Flow Fragment(Bits(codedBitNum bits))
  val dataOut = master Flow Fragment(Vec(discrepancyType, numStates))

  // build ROM
  val updatingMatrices: Seq[MinplusMatrix] = MinplusMatrix.trellis2Minplus(trellis, Algos.Hamming)
  val data: Seq[Array[Double]] = updatingMatrices.map(_.value.flatten.filter(_ < MinplusMatrix.max))
  val hardData: Seq[Vec[UInt]] = data.map(_.map(_.toInt).map(U(_, incrementWidth bits))).map(Vec(_))
  val updatingROM: Mem[Vec[UInt]] = Mem(hardData)
  //  updatingROM.addAttribute("rom_style", "block") // or, this would be implemented as a big MUX
  // TODO: this could be shared by multiple ACS module
  val updatingValue: Vec[UInt] = updatingROM.readSync(dataIn.fragment.asUInt)

  // regs for current discrepancy
  val discrepancies = Vec(Reg(discrepancyType), numStates)
  discrepancies.head.init(discrepancyType().getZero) // starts from all-0 state
  discrepancies.tail.foreach(_.init(discrepancyBig))

  // ACS represented in Minplus Algebra matrix multiplication
  val discrepanciesNext: Vec[UInt] = Vec(discrepancyType, numStates)
  val vector: Array[Vec[UInt]] = Array(discrepancies) // 1 * numState vector
  val matrix: Array[Array[Double]] = updatingMatrices.head.value // numState * numState matrix
  val sparse2dense: Map[(Int, Int), Int] = Array.tabulate(numStates, numStates)((r, c) => (r, c, matrix(r)(c))).flatten.filter(_._3 < MinplusMatrix.max)
    .zipWithIndex.map { case ((r, c, value), i) => (r, c) -> i }.toMap
  //  println(sparse2dense.mkString(" "))

  //  printlnGreen(nextStates.map(_.mkString(" ")) .mkString("\n"))

  val ret = Array.tabulate(1, numStates) { (i, k) =>
    // pick up signals
    val coords = Array.tabulate(numStates)(j => ((i, j), (j, k)))
    val validCoords = coords.filter(coord => sparse2dense.isDefinedAt(coord._2))
    val pairs = validCoords.map(coord => (vector(coord._1._1)(coord._1._2), updatingValue(sparse2dense(coord._2))))
    //    println("cands")
    //    println(validCoords.map(_._1).mkString(" "))
    //    println(validCoords.map(_._2).mkString(" "))
    //    println(validCoords.map(_._2).map(sparse2dense(_)).mkString(" "))
    // addition
    val afterA = pairs.map { case (dis, inc) => dis + inc }
    // compare & select
    val CS = (a: UInt, b: UInt) => Mux(a < b, a, b)
    val afterCS: UInt = Vec(afterA).reduceBalancedTree(CS)
    afterCS
  }.flatten
  discrepanciesNext := Vec(ret)

  // control logic
  //  def disNextLatency = LatencyAnalysis(dataIn.fragment, discrepanciesNext.head)
  def disNextLatency = 1

  val delayedValid = Delay(dataIn.valid, disNextLatency, init = False)
  val delayedLast = Delay(dataIn.last, disNextLatency, init = False)

  when(delayedValid && !delayedLast)(discrepancies := discrepanciesNext) // continuous updating
    .otherwise {
      discrepancies.head := discrepancyType().getZero // starts from all-0 state
      discrepancies.tail.foreach(_ := discrepancyBig)
    }

  dataOut.valid := RegNext(delayedValid, init = False)
  dataOut.last := RegNext(delayedLast, init = False)
  dataOut.fragment := Mux(dataOut.last, RegNext(discrepanciesNext), discrepancies)
}

object ViterbiForwarding {
  def main(args: Array[String]): Unit = {
    val trellis = Trellis.poly2trellis(7, Array(177, 131))
    //    GenRTL(ViterbiForwarding(trellis))
    VivadoSynth(ViterbiForwarding(trellis), name = "viterbiForwarding")
  }
}
