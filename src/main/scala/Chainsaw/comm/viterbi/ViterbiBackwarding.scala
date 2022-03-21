package Chainsaw.comm.viterbi

import Chainsaw._
import spinal.core._
import spinal.lib._

case class ViterbiBackwarding(trellis: Trellis[Int]) extends Component {

  import trellis._

  val decodedBitNum = log2Up(numInputSymbols)
  val stateWidth    = log2Up(numStates)
  val stateType     = HardType(UInt(stateWidth bits))

  val discrepancyWidth = 4
  val discrepancyType  = HardType(UInt(discrepancyWidth bits)) // TODO: parameterize the size
  val discrepancyBig   = U(1 << (discrepancyWidth - 1), discrepancyWidth bits)

  val dataIn     = slave Flow Fragment(Vec(discrepancyType, numStates))
  val stateStart = in(stateType())
  val dataOut    = master Flow Fragment(Bits(decodedBitNum bits))

  val currentState = Reg(stateType())
  currentState.init(stateType().getZero)

  // pick up the data for comparison
  val prevStates: Seq[Array[Int]] = (0 until numStates).map(next => lookBackMap(next).map(_._1).toArray)
  val candidateDiscrepancies      = Vec(discrepancyType, numInputSymbols)
  val candidateState              = Vec(stateType, numInputSymbols)
  switch(currentState) {
    (0 until numStates).foreach { state =>
      is(state) {
        candidateDiscrepancies.zip(candidateState).zip(prevStates(state)).foreach { case ((disPort, statePort), prevState) =>
          disPort   := dataIn.fragment(prevState)
          statePort := U(prevState, stateWidth bits)
        }
      }
    }
  }
  // get the state with
  val CS = (a: Bits, b: Bits) => {
    val (disa, statea) = a.splitAt(stateWidth)
    val (disb, stateb) = b.splitAt(stateWidth)
    Mux(disa.asUInt < disb.asUInt, statea, stateb)
  }
  val cands: Vec[Bits]   = Vec(candidateDiscrepancies.zip(candidateState).map { case (dis, state) => (dis @@ state).asBits })
  val prevStateMin: Bits = cands.reduceBalancedTree(CS)

  // control logic
  when(dataIn.valid.rise() || dataIn.last)(currentState := stateStart) // continuous updating
    .otherwise(currentState := prevStateMin.asUInt)

  dataOut.fragment := currentState(stateWidth - 1 downto stateWidth - decodedBitNum).asBits // the MSBs form the input symbol
  dataOut.valid    := RegNext(dataIn.valid, init = False)
  dataOut.last     := RegNext(dataIn.last, init = False)
}

object ViterbiBackwarding {
  def main(args: Array[String]): Unit = {
    val trellis = Trellis.poly2trellis(7, Array(177, 131))
    VivadoSynth(ViterbiBackwarding(trellis))
    //    VivadoImpl(ViterbiBackwarding(trellis))
  }
}
