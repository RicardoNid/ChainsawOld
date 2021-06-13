package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

// TODO: improve the type consistency

case class MontMulPEData(w: Int) extends Bundle {
  // SComp ## S0 combines a SWord
  val S0 = UInt(1 bits) // 0
  val SComp = UInt(w - 1 bits) // w-1 downto 1
  val YWord = UInt(w bits)
  val MWord = UInt(w bits)
}

case class MontMulControl() extends Bundle {
  // xi should be externally registered
  // short as it is, it is a UInt as it has numeric semantic
  val xi = UInt(1 bits)
  val SetXi = Bool()
  val run = Bool() // the overall control
}

/**
 * @param w the word size of MontMul
 */
class MontMulPE(w: Int) extends Component { // we want it to be synthesized independently

  val io = new Bundle {
    val dataIn = in(MontMulPEData(w))
    val dataOut = out(MontMulPEData(w))
    val controlIn = in(MontMulControl())
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
  when(io.controlIn.SetXi) {
    xiInUse := io.controlIn.xi
    xi := io.controlIn.xi
    qiInUse := ((io.dataIn.YWord.lsb & xiInUse.asBool) ^ io.dataIn.S0.lsb)
    qi := ((io.dataIn.YWord.lsb & xiInUse.asBool) ^ io.dataIn.S0.lsb)
  }.otherwise {
    xiInUse := xi
    qiInUse := qi
  }

  // intermediate signals
  val xiYWord = Mux(xiInUse.asBool, U(0), io.dataIn.YWord)
  val qiMWord = Mux(qiInUse, U(0), io.dataIn.MWord)
  val C = Mux(io.dataIn.S0.asBool, CO, CE)

  val SumLower = (xiYWord(w - 2 downto 0) +^ C) +^ // w + 1 bits
    (qiMWord(w - 2 downto 0) +^ io.dataIn.SComp)
  val HigherCommonPart = io.dataIn.YWord.msb.asUInt +^ io.dataIn.MWord.msb.asUInt // 2 bits
  val SumHigherOdd = (U(1, 1 bits) +^ SumLower(w downto w - 1)) +^ HigherCommonPart
  val SumHigherEven = SumLower(w downto w - 1) +^ HigherCommonPart

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
}

case class Time(hour: Int, minute: Int, second: Int)

/**
 * @param n size of the MontMul
 * @param w word size of the MontMulPE
 * @param p number of the MontMulPE
 */
class MontMulSystolic(n: Int, w: Int, p: Int) extends Component {

  import scala.math.ceil

  val e = ceil((n + 1).toDouble / w).toInt // number of words

  val io = new Bundle {
    val run = in Bool()
    val setXi = in Bits (p bits)
    val xiIn = in UInt (1 bits)
    val YWordIn = in UInt (w bits)
    val MWordIn = in UInt (w bits)
    val SWordIn = in UInt (w bits)
    val SWordOut = out UInt (w bits)
    //    val validOut = out Bool()
  }

  val PEs = (0 until p).map(_ => new MontMulPE(w))
  // connecting PEs with each other
  PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.dataIn := prev.io.dataOut }
  // PE control inputs
  PEs.zipWithIndex.foreach { case (e, i) =>
    e.io.controlIn.run := io.run
    e.io.controlIn.xi := io.xiIn
    e.io.controlIn.SetXi := io.setXi(i)
  }
  // PE data input
  PEs(0).io.dataIn.S0 := io.SWordIn.lsb.asUInt
  PEs(0).io.dataIn.SComp := io.SWordIn(w - 1 downto 1)
  PEs(0).io.dataIn.MWord := io.MWordIn
  PEs(0).io.dataIn.YWord := io.YWordIn

  io.SWordOut := (PEs.last.io.dataOut.SComp ## PEs.last.io.dataOut.S0).asUInt
}

object MontMulSystolic {
  def main(args: Array[String]): Unit = {
    GenRTL(new MontMulSystolic(8, 4, 8))
    SimConfig.withWave.compile(new MontMulSystolic(8, 4, 8)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
      // MontMul(159,148), modulus = 177
      (0 until 8).foreach { i =>
        io.run #= true
        io.setXi #= (BigInt(1) << i)
        io.xiIn #= BigInt(159).toString(2).reverse(i).asDigit
        if (i == 0) {
          io.MWordIn #= BigInt(1) // 177 % 16
          io.YWordIn #= BigInt(4) // 148 % 16
        } else if (i == 1) {
          io.MWordIn #= BigInt(11) // 177 / 16
          io.YWordIn #= BigInt(9) // 148 / 16
        }
        clockDomain.waitSampling()
      }
    }
  }
}
