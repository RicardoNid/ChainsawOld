package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class ParallelConvenc extends DSPDUTTiming[Vec[Bits], Bits] {
  val start = in Bool()
  override val input: Vec[Bits] = in Vec(Bits(448 bits), 2)

  val input0 = Bits(448 bits)
  val input1 = Bits(448 bits)
  val convencs0 = (0 until 448).map(i => Convenc(input0(i), ConvencConfig(7, Array(171, 133))))
  val convencs1 = (0 until 448).map(i => Convenc(input1(i), ConvencConfig(7, Array(171, 133))))
  val convCoded = Bits(896 bits)
  val valid = out(Bool)

  val fsm = new StateMachine {

    val IDLE = new State() with EntryPoint
    IDLE.whenIsActive(when(start)(goto(INIT)))

    val INIT = new StateDelay(6)
    INIT.whenCompleted(goto(RUNNING0))
    INIT.whenIsActive()

    val RUNNING0 = new StateDelay(16)
    val RUNNING1 = new StateDelay(16)
    RUNNING0.whenCompleted(goto(RUNNING1))
    RUNNING1.whenCompleted(goto(RUNNING0))

    convCoded := Mux(
      isActive(RUNNING0),
      convencs0.map(_.implicitValue).flatten.asBits(),
      convencs1.map(_.implicitValue).flatten.asBits())
    valid := isActive(RUNNING0) || isActive(RUNNING1)

    when(isActive(RUNNING0))(input0 := input(0)) // caution: vector generated from asBools starts from the LSB
      .otherwise(input0 := (input(0).asBools.tail :+ False).asBits())

    when(isActive(RUNNING1))(input1 := input(1))
      .otherwise(input1 := (input(1).asBools.tail :+ False).asBits())
  }

  override val output: Bits = out(convCoded)

  override val timing: TimingInfo = TimingInfo(70, 64, 7, 128)
}

object ParallelConvenc {
  def main(args: Array[String]): Unit = {
    GenRTL(new ParallelConvenc)
    VivadoSynth(new ParallelConvenc)
  }
}
