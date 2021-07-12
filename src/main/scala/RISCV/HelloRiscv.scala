package RISCV

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real


class HelloRiscv(testCases: Seq[BigInt]) extends Component {

  val inst = BigInt("1" * 32, 2)

  val input = in Bits (32 bits)
  when(input === Riscv.ADDI) { // ADDI
    Riscv.IMM(input).b_sext
  }

  val IM = Mem(Bits(32 bits), initialContent = testCases.map(B(_)))
  val DM = Mem(Bits(32 bits), 1 << 8)
  val RF = Vec(Reg(Bits(32 bits)), 32) // 1W2R
}

object HelloRiscv {
  def main(args: Array[String]): Unit = {
    println(Riscv.ADDI)
  }
}
