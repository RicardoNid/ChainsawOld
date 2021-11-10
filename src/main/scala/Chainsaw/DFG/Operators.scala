package Chainsaw.DFG

import Chainsaw._
import spinal.core._

// common nodes which we can build from the op directly
case class BinaryHardware[T <: Data](op: (T, T) => T, width: BitCount = -1 bits)
  extends DSPHardware[T](impl = (dataIns: Seq[T], _: GlobalCount) => Seq(op(dataIns(0), dataIns(1))), inDegree = 2, outWidths = Seq(width))

class BinaryNode[T <: Data](op: (T, T) => T, width: BitCount = -1 bits, name: String, delay: CyclesCount, exeTime: TimeNumber)
  extends DeviceNode[T](BinaryHardware(op, width), name, delay, exeTime) {
  override def copy(newName: String): BinaryNode[T] = new BinaryNode(op, width, newName, delay, exeTime)
}

object BinaryNode {
  def apply[T <: Data](op: (T, T) => T, name: String, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): BinaryNode[T] = new BinaryNode(op, width, name, delay, exeTime)
}

case class TrinaryHardware[T <: Data](op: (T, T, T) => T, width: BitCount = -1 bits)
  extends DSPHardware[T](impl = (dataIns: Seq[T], _: GlobalCount) => Seq(op(dataIns(0), dataIns(1), dataIns(2))), inDegree = 3, outWidths = Seq(width))

class TrinaryNode[T <: Data](op: (T, T, T) => T, width: BitCount = -1 bits, name: String, delay: CyclesCount, exeTime: TimeNumber)
  extends DeviceNode[T](TrinaryHardware(op, width), name, delay, exeTime) {
  override def copy(newName: String): TrinaryNode[T] = new TrinaryNode(op, width, newName, delay, exeTime)
}

object TrinaryNode {
  def apply[T <: Data](op: (T, T, T) => T, name: String, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): TrinaryNode[T] =
    new TrinaryNode(op, width, name, delay, exeTime)
}

/** Butterfly hardware takes two input and a coefficient, generates two output, they're heavily used as building blocks of more complicated DFG
 */
case class ButterflyHardware[THard <: Data](op: (THard, THard, THard) => (THard, THard), width: BitCount = -1 bits) // (a, b, coeff)
  extends DSPHardware[THard](impl =
    (dataIns: Seq[THard], _: GlobalCount) => {
      val ret = op(dataIns(0), dataIns(1), dataIns(2))
      Seq(ret._1, ret._2)
    },
    inDegree = 3, outWidths = Seq(width, width))

class ButterflyNode[T <: Data](op: (T, T, T) => (T, T), width: BitCount = -1 bits, name: String, delay: CyclesCount, exeTime: TimeNumber)
  extends DeviceNode[T](ButterflyHardware(op, width), name, delay, exeTime) {
  override def copy(newName: String): ButterflyNode[T] = new ButterflyNode(op, width, newName, delay, exeTime)
}

object ButterflyNode {
  def apply[T <: Data](op: (T, T, T) => (T, T), name: String, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): ButterflyNode[T] =
    new ButterflyNode(op, width, name, delay, exeTime)
}

/** commonly used operators
 *
 */
object Operators {

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String, delay: CyclesCount, exeTime: TimeNumber): DeviceNode[T] = DeviceNode(hardware, name, delay, exeTime)
  }

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
