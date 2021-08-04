package FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import matlabIO._

import scala.collection.mutable.ArrayBuffer

class Interleaver() extends Component {

  val run = in Bool()
  val we = in Bool()

  val dataIn = in Bits (4 bits)
  val dataOut = out Bits (4 bits)

  val rams = Seq.fill(4)(Mem(Bits(1 bits), 4)) // mapped to 32 RAM32M16, each consumes 8 LUTs
  val count = Counter(4, inc = run)

  val dataInShifted: Bits = dataIn.rotateRight(count.value)
  val dataInPacked: Vec[Bits] = dataInShifted.subdivideIn(4 slices)

  val outputAddresses = Vec(Reg(UInt(log2Up(4) bits)), 4)
  outputAddresses.zipWithIndex.foreach { case (addr, i) => addr.init(i) }

  val dataOutPacked = Vec(Bits(1 bits), 4)
  val dataOutShifted = dataOutPacked.reverse.asBits.rotateLeft(count.value)

  dataOutPacked.foreach(_.clearAll())
  when(run) {
    when(we) {
      (0 until 4).foreach(i => rams(3 - i)(count.value) := dataInPacked(i))
    }.otherwise {
      (0 until 4).foreach(i => dataOutPacked(i) := rams(i).readAsync(outputAddresses(i)))
      outputAddresses.zip(outputAddresses.tail :+ outputAddresses.head).foreach { case (left, right) => right := left }
    }
  }

  dataOut := dataOutShifted
}

object Interleaver extends App {
  GenRTL(new Interleaver)
  SimConfig.withWave.compile(new Interleaver).doSim { dut =>
    import dut._

    val eng = AsyncEng.get()
    clockDomain.forkStimulus(2)

    run #= false
    we #= false
    clockDomain.waitSampling()

    val inputBlock = ArrayBuffer[Int]()
    val outputBlock = ArrayBuffer[Int]()

    (0 until 4).foreach { i =>
      run #= true
      val input = DSPRand.nextInt(16)
      dataIn #=  input
      inputBlock ++= input.toBinaryString.padToLeft(4, '0').map(_.asDigit)
      we #= true
      clockDomain.waitSampling()
    }

    (0 until 4).foreach { i =>
      run #= true
      we #= false
      clockDomain.waitSampling()
      outputBlock ++= dataOut.toBigInt.toString(2).padToLeft(4, '0').map(_.asDigit)
    }

    println(inputBlock.grouped(4).toArray.map(_.mkString("")).mkString("\n"))
    printlnGreen("golden")
    val golden = eng.feval[Array[Int]]("matintrlv", inputBlock.toArray, Array(4), Array(4))
    println(golden.grouped(4).toArray.map(_.mkString("")).mkString("\n"))
    printlnGreen("yours")
    val yours = outputBlock
    println(outputBlock.grouped(4).toArray.map(_.mkString("")).mkString("\n"))

    assert(golden.mkString("") == yours.mkString(""))
  }
  //  VivadoSynth(new Interleaver)
}
