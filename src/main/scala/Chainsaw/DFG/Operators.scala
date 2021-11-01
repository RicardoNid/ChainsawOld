package Chainsaw.DFG

import spinal.core._

object Operators {

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String, delay: CyclesCount, exeTime: TimeNumber): GeneralNode[T] = GeneralNode(hardware, name, delay, exeTime)
  }

  // logical operators
  val and: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a & b
  val or: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a | b
  val xor: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a ^ b

  // sint operators
  val sintAdd: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a + b
  val sintMult: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a * b
  val sintMultAdd: (SInt, SInt, SInt) => SInt = (a: SInt, b: SInt, c: SInt) => (a * b).resized + c

  // bit/digit level operators
  val sintAddC: (SInt, SInt, SInt) => Seq[SInt] = (a: SInt, b: SInt, c: SInt) => {
    val result = a +^ b + c
    Seq(result(result.getBitsWidth - 2 downto 0), result.msb.asSInt)
  }

  // operators mapped to Xilinx DSP slice
  def sintMACDSP(delay: Int): (SInt, SInt, SInt) => SInt = (a: SInt, b: SInt, c: SInt) => {
    val ret = if (delay == 0) a * b + c else RegNext(a * b) + c
    ret.addAttribute("use_dsp", "yes")
    ret
  }

  /** An operator which acts as a passthrough, whether it is of hardware or software
   */
  def passThrough[T](width: BitCount = -1 bits): DSPHardware[T] = DSPHardware(
    impl = (dataIns: Seq[T], _: GlobalCount) => dataIns,
    inDegree = 1,
    outWidths = Seq(width)
  )

  // adder with carry
  class AdderC[THard <: Data](op: (THard, THard, THard) => Seq[THard], width: Seq[BitCount] = Seq(-1 bits), name: String, delay: CyclesCount, exeTime: TimeNumber)
    extends GeneralNode[THard](DSPHardware((dataIns: Seq[THard], _: GlobalCount) => op(dataIns(0), dataIns(1), dataIns(2)), 3, width), name, delay, exeTime) with Foldable[THard] {
    require(width.size == 2)

    override def copy(newName: String): AdderC[THard] = new AdderC(op, width, newName, delay, exeTime)
  }

  object AdderC {
    def apply[THard <: Data](op: (THard, THard, THard) => Seq[THard], name: String, width: Seq[BitCount], delay: CyclesCount, exeTime: TimeNumber): AdderC[THard] = new AdderC(op, width, name, delay, exeTime)
  }
}
