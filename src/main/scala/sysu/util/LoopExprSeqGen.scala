package sysu.util

import spinal.core._
import sysu.Opt.LoopExpr

class LoopExprSeqGen(loopExpr: LoopExpr) extends Component{

  val bitWidth = log2Up(loopExpr.max + 1)

  val io = new Bundle {
    val output = out UInt(bitWidth bits)
  }

  val counters = loopExpr.variable.map{case(name, param) => new BetterCounter(param)}
  io.output := counters.map(_.io.output).reduce(_ + _) + U(loopExpr.constant, bitWidth bits)
}