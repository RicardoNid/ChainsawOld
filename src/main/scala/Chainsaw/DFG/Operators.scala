package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

/** commonly used operators
 *
 */
object Operators {

  implicit class hardware2Node[T <: Data](hardware: DSPHardware[T]) {
    def asDSPNode(name: String): DeviceNode[T] = DeviceNode(name, hardware)
  }

  /** generate a pass through hardware of given width
   */
  def passThrough[T <: Data](width: BitCount = -1 bits): DSPHardware[T] = DSPHardware(
    impl = (dataIns: Seq[T], _: GlobalCount) => dataIns,
    inDegree = 1,
    outWidths = Seq(width),
    delay = 0 cycles,
    exeTime = 0 sec
  )

  // binary operators

  // logical operators
  val and: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a & b
  val or: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a | b
  val xor: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a ^ b

  // sint operators
  val sintAdd: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a + b
  val sintMult: (SInt, SInt) => SInt = (a: SInt, b: SInt) => a * b
  val sintMultAdd: (SInt, SInt, SInt) => SInt = (a: SInt, b: SInt, c: SInt) => (a * b) + c

  // operators of ''Computer Arithmetic''

  /** signed adder with carry
   */
  def sintAddCNode(width: BitCount): DeviceNode[SInt] = DeviceNode("sintAdderC",
    DSPHardware(impl =
      (dataIns: Seq[SInt], _: GlobalCount) => {
        val result = dataIns(0) +^ dataIns(1) + dataIns(2)
        Seq(result(result.getBitsWidth - 2 downto 0), result.msb.asSInt)
      }, 3, Seq(width, 1 bits), 0 cycles, 0 sec)
  )

  // operators who has a checked device mapping

  /** a operator using DSP48 to finish multiply-accumulate
   *
   * @param mreg whether pipeline the multiplication result or not
   */
  @xilinxDevice
  def macDSP48(mreg: Boolean = true): TrinaryNode[SInt] = {
    val op = (coeff: SInt, data: SInt, partialSum: SInt) => {
      val ret = if (mreg) RegNext(coeff * data) + partialSum else coeff * data + partialSum
      ret.addAttribute("use_dsp", "yes")
    }
    TrinaryNode("macDSP48", op, delay = (if (mreg) 1 else 0) cycles)
  }

}
