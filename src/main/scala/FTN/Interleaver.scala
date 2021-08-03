package FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

class Interleaver extends Component {

  val run = in Bool()
  val we = in Bool()

  val dataIn = in Bits (128 bits)
  val dataOut = out Bits (128 bits)

  val rams = Seq.fill(32)(Mem(Bits(4 bits), 32)) // mapped to 32 RAM32M16, each consumes 8 LUTs
  val count = Counter(32, inc = run)

  // (0, 32, 64, 96) (1, 33, 65, 97)...
  val dataInShifted: Bits = dataIn.rotateLeft(count.value << 2)
  val dataInPacked: Vec[Bits] = Vec(
    (0 until 32).map(i =>
      (0 until 4).map(j =>
        dataInShifted(128 - 1 - (i + j * 32)).asBits).reduce(_ ## _)))

  // as all these regs need initialization, they won't be mapped to SRL
  // however, if we use BOOT type initialization?
  val outputAddresses = Vec(Reg(UInt(log2Up(32) bits)), 32)
  outputAddresses.zipWithIndex.foreach { case (addr, i) => addr.init(i) }

  val dataOutPacked = Vec(Bits(4 bits), 32)
  // 0,1,2... ->
  val dataOutShifted = dataOutPacked.asBits.rotateLeft(count.value << 2)

  dataOutPacked.foreach(_.clearAll())
  when(run) {
    when(we) {
      (0 until 32).foreach(i => rams(i)(count.value) := dataInPacked(i))
    }.otherwise {
      (0 until 32).foreach(i => dataOutPacked(i) := rams(i).readAsync(outputAddresses(i)))
      outputAddresses.zip(outputAddresses.tail :+ outputAddresses.head).foreach { case (left, right) => right := left }
    }
  }

  dataOut := dataOutShifted
}

object Interleaver extends App {
  GenRTL(new Interleaver)
  SimConfig.withWave.compile(new Interleaver).doSim { dut =>
    import dut._
    clockDomain.forkStimulus(2)

    run #= false
    we #= false
    clockDomain.waitSampling()

    (0 until 32).foreach { i =>
      run #= true
      val bytes = Array.fill(16)(1.toByte)
      DSPRand.nextBytes(bytes)
      dataIn #= BigInt(bytes).abs
      we #= true
      clockDomain.waitSampling()
    }

    (0 until 32).foreach { i =>
      run #= true
      dataIn #= BigInt(1) << (i * 4 + 3)
      we #= false
      clockDomain.waitSampling()
    }
  }
//  VivadoSynth(new Interleaver)
}
