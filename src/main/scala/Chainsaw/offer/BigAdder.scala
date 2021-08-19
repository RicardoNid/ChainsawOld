package Chainsaw.offer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class BigAdder(opSize: Int, wordSize: Int,
                    parallel: Boolean) extends Component {

  require(Seq(opSize, wordSize).forall(isPow2(_)))

  val latency = opSize / wordSize
  val throughput = if (parallel) 1 else wordSize / opSize

  val ioSize = if (parallel) opSize else wordSize

  val dataIn = slave Flow Fragment(Vec(UInt(ioSize bits), 2))
  val dataOut = master Flow Fragment(UInt(ioSize bits))

  parallel match {

    case true => {

      dataOut.valid := Delay(dataIn.valid, latency, init = False)
      dataOut.last := False // will be trimmed

      val adderCount = opSize / wordSize
      val op0s = dataIn.payload(0).subdivideIn(adderCount slices)
      val op1s = dataIn.payload(1).subdivideIn(adderCount slices)
      val inputDelays = (0 until adderCount).map(i => Delay(Vec(op0s(i), op1s(i)), i))
      val partials = Seq.fill(adderCount)(Reg(UInt(wordSize + 1 bits)))

      partials(0) := inputDelays(0)(0) +^ inputDelays(0)(1)
      (1 until adderCount).zip(partials.init.zip(partials.tail)).foreach { case (i, (lower, higher)) =>
        higher := inputDelays(i)(0) +^ inputDelays(i)(1) + lower.msb.asUInt
      }

      val outputDelays = partials.zipWithIndex.map { case (partial, i) =>
        Delay(partial(wordSize - 1 downto 0), adderCount - i - 1)
      }

      dataOut.payload := outputDelays.asBits().asUInt
    }
    case false =>
      dataOut.valid := RegNext(dataIn.valid, init = False)
      dataOut.last := RegNext(dataIn.last, init = False)
      val partial = RegInit(U(0, wordSize + 1 bits))
      when(dataIn.last) {
        partial(wordSize - 1 downto 0) := dataIn.payload(0) + dataIn.payload(1) + partial.msb.asUInt
        partial.msb := False
      }.otherwise(partial := dataIn.payload(0) +^ dataIn.payload(1) + partial.msb.asUInt)
      dataOut.payload := partial.resize(wordSize)
  }


  println(s"latency: $latency, throughput: $throughput")
}

object BigAdder extends App {
  SimConfig.withWave.compile(new BigAdder(256, 8, false)).doSim { dut =>

    dut.clockDomain.forkStimulus(2)
    dut.dataIn.valid #= false
    dut.dataIn.last #= false
    dut.clockDomain.waitSampling()
    dut.clockDomain.waitSampling()

    val bytes = Array.fill(32)(0.toByte)
    def nextBigInt() = {
      DSPRand.nextBytes(bytes)
      BigInt(bytes).abs
    }
    val op0 = nextBigInt()
    val op1 = nextBigInt()

    def byte2unsigned(bt: Byte): Int = bt.toBinaryString.padToLeft(8, '0').takeRight(8).reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum

    if (dut.parallel) {
      dut.dataIn.payload(0) #= op0
      dut.dataIn.payload(1) #= op1
      dut.dataIn.valid #= true
      dut.dataIn.last #= true
      dut.clockDomain.waitSampling()
    }
    else {
      (0 until 32).foreach { i =>
        dut.dataIn.payload(0) #= byte2unsigned(op0.toByteArray(31 - i))
        dut.dataIn.payload(1) #= byte2unsigned(op1.toByteArray(31 - i))
        dut.dataIn.valid #= true
        dut.dataIn.last #= (i == 31)
        dut.clockDomain.waitSampling()
      }
    }
    dut.dataIn.valid #= false
    dut.dataIn.last #= false

    dut.clockDomain.waitSampling(dut.latency)

    println((op0 + op1).showWordsHex(8).mkString(" "))
  }
}
