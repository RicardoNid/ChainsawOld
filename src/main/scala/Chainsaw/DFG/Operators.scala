package Chainsaw.DFG

import Chainsaw._
import spinal.core._

object Operators {

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String, delay: CyclesCount, exeTime: TimeNumber): DeviceNode[T] = DeviceNode(hardware, name, delay, exeTime)
  }

  // software operators


  // logical operators
  val and: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a & b
  val or: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a | b
  val xor: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a ^ b

  // sint operators
  val sintAdd: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a + b
  val sintMult: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a * b
  val sintMultAdd: (SInt, SInt, SInt) => SInt = (a: SInt, b: SInt, c: SInt) => ((a * b) + c)

  // bit/digit level operators
  val sintAddC: (SInt, SInt, SInt) => Seq[SInt] = (a: SInt, b: SInt, c: SInt) => { // adder take and
    val result = a +^ b + c
    Seq(result(result.getBitsWidth - 2 downto 0), result.msb.asSInt)
  }

  /** a operator using DSP48 to finish multiply-accumulate
   *
   * @param mreg whether pipeline the multiplication result or not
   */
  @xilinxDevice
  def macDSP48(mreg: Boolean = true): TrinaryNode[SInt] = {
    val op = (coeff: SInt, data: SInt, partialSum: SInt) =>
      if (mreg) RegNext(coeff * data) + partialSum else coeff * data + partialSum
    TrinaryNode(op, "macDSP48", delay = (if (mreg) 1 else 0) cycles)
  }

  // operators mapped to Xilinx DSP slice
  def sintMACDSP(delay: Int): (SInt, SInt, SInt) => SInt = (a: SInt, b: SInt, c: SInt) => {
    val prod = RegNext(a * b)
    prod.init(0)
    val ret = if (delay == 0) a * b + c else prod + c
    ret.addAttribute("use_dsp", "yes")
    ret
  }

  /** An operator which acts as a passthrough, whether it is of hardware or software
   */
  def passThrough[T <: Data](width: BitCount = -1 bits): DSPHardware[T] = DSPHardware(
    impl = (dataIns: Seq[T], _: GlobalCount) => dataIns,
    inDegree = 1,
    outWidths = Seq(width)
  )

  // ??? nonsense
  // adder with carry
  class AdderC[THard <: Data](op: (THard, THard, THard) => Seq[THard], name: String, width: Seq[BitCount], delay: CyclesCount, exeTime: TimeNumber)
    extends DeviceNode[THard](DSPHardware((dataIns: Seq[THard], _: GlobalCount) => op(dataIns(0), dataIns(1), dataIns(2)), 3, width), name, delay, exeTime) {
    require(width.size == 2)

    override def copy(newName: String): AdderC[THard] = new AdderC(op, newName, width, delay, exeTime)
  }

  object AdderC {
    def apply[THard <: Data](op: (THard, THard, THard) => Seq[THard], name: String, width: Seq[BitCount] = Seq(-1 bits, -1 bits), delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): AdderC[THard] =
      new AdderC(op, name, width, delay, exeTime)
  }
}
