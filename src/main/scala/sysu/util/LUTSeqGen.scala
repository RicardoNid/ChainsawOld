package sysu.util

import spinal.core._
import spinal.core.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

import scala.util.Random

class LUTSeqGen(sequence: Seq[Int]) extends Component {

  val length = sequence.length
  val bitWidth = log2Up(sequence.max + 1)

  val io = new Bundle {
    val output = out UInt (bitWidth bits)
  }

  val sequenceVec = Vec(UInt(bitWidth bits), length)
  (0 until length).foreach(i => sequenceVec(i) := U(sequence(i)).resized)

  val rom = Mem(UInt(bitWidth bits), sequenceVec)

  val sequenceReg = Reg(UInt(bitWidth bits)) init (sequence(0))
  val addrCount = Counter(length, True)

  io.output := rom(addrCount)
}

object testLUTSeqGen {

  val period = 2

  val randGen = new Random(42)
  val randomSeq = Array.ofDim[Int](100).map(_ => randGen.nextInt % 100 + 500)

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new LUTSeqGen(randomSeq)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          sleep(1000 * period)
          simSuccess()
        }
      }
  }
}