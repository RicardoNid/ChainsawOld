package CNN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.simple.PipelinedMemoryBus

import scala.util.Random

class AdderTree(bitWidth: Int = 8,
                numOperand: Int = 16,
                pipelined: Boolean = false) extends Component {

  //  require(isPow2(numOperand), "number of operands should be power of 2")

  val outWidthGrowth = log2Up(numOperand)

  val io = new Bundle {
    val operands = in Vec(UInt(bitWidth bits), numOperand)
    val result = out UInt ((bitWidth + outWidthGrowth) bits)
  }

  def tree(op: IndexedSeq[UInt]): IndexedSeq[UInt] = {
    val n = op.length
    if (n == 1) op
    else {
      val half = (n + 1) / 2
      val mid = Range(0, half).map(i => if (n % 2 == 1 && i == half - 1) op(i) else op(i) +^ op(i + half))
      mid.foreach(signal => signal.addAttribute("dont_touch = \"yes\""))
      if (pipelined) tree(mid.map(op => RegNext(op))) else tree(mid)
    }
  }

  io.result := tree(io.operands)(0).resized
}

object AdderTree {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new AdderTree(8, 13, true))
  }
}

object testAdderTree {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new AdderTree(4)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          simSuccess()
        }
      }
  }
}
