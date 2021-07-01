package Chainsaw.Crypto.RSA

import Chainsaw.{DSPDUTTiming, TimingInfo}
import spinal.core.{RegNext, SInt, UInt, Vec, in, out, _}
import spinal.core._
import spinal.core.sim._
import spinal.lib._

class BigSub(n: Int, val latency: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  val ret = (input(0).resize(n + 1).asSInt - input(1).resize(n + 1).asSInt).asUInt
  override val output: UInt = out(Delay(ret, latency)) // the output is n + 1 bit long
  override val timing: TimingInfo = TimingInfo(1, 1, latency, 1)
  def doSub(a: UInt, b: UInt): Unit = {
    input(0) := a.resized
    input(1) := b.resized
  }
}

class BigAdd(n: Int, val latency: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  val ret = input(0) +^ input(1)
  override val output: UInt = out(Delay(ret, latency)) // the output is n + 1 bit long
  override val timing: TimingInfo = TimingInfo(1, 1, latency, 1)
  def doAdd(a: UInt, b: UInt): Unit = {
    input(0) := a.resized
    input(1) := b.resized
  }
}

class BigAddSub(n: Int, val latency: Int) extends DSPDUTTiming[Vec[UInt], UInt] {

  //  val drop = RegInit(True)
  //  drop := !drop

  val subSize = n / 2 + 1
  val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  val isAdd = in Bool()
  val ret = Mux(isAdd,
    input(0) +^ input(1),
    (input(0)(subSize downto 0).asSInt - input(1)(subSize downto 0).asSInt).asUInt.resized) // TODO: reconsider the sub part
  //  val output: UInt = out(Mux(drop, U(0, ret.getBitsWidth bits), Delay(ret, latency)))
  val output: UInt = out(Delay(ret, latency))
  override val timing: TimingInfo = TimingInfo(1, 1, latency, 1)
  def doAdd(a: UInt, b: UInt) = {
    input(0) := a.resized
    input(1) := b.resized
    isAdd := True
  }
  def doSub(a: UInt, b: UInt): Unit = {
    require(a.getBitsWidth == n / 2 + 1)
    require(b.getBitsWidth == n / 2)
    input(0) := a.resized
    input(1) := b.resized
    isAdd := False
  }
}

object BigAddSub {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new BigAddSub(4, 0)).doSim { dut =>
      Array.tabulate(4, 4)((_, _)).flatten.foreach { pair =>
        dut.input(0) #= pair._1
        dut.input(1) #= pair._2
        sleep(1)
      }
    }
  }
}

class BigMult(n: Int, val latency: Int) extends DSPDUTTiming[Vec[UInt], UInt] {

  //  val drop = RegInit(False)
  //  drop := !drop

  override val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  val ret = input(0) * input(1)
  //  override val output: UInt = out(Mux(drop, U(0, ret.getBitsWidth bits), Delay(ret, latency)))
  override val output: UInt = out(Delay(ret, latency))
  override val timing: TimingInfo = TimingInfo(1, 1, latency, 1)
  def doMult(a: UInt, b: UInt): Unit = {
    input(0) := a.resized
    input(1) := b.resized
  }
}