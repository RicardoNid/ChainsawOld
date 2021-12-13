package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

/** commonly used operators
 *
 */
object Operators {

  /** generate a pass through hardware of given width
   */
  def passThrough[T <: Data](width: BitCount = -1 bits): UnaryHardware[T] = UnaryHardware(
    op = (dataIns: T) => dataIns,
    width = width,
    delay = 0 cycles,
    exeTime = 0 sec
  )

  // logical operators
  val and: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a & b
  val or: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a | b
  val xor: (Bits, Bits) => Bits = (a: Bits, b: Bits) => a ^ b

  // sint operators
  val sintInc: SInt => SInt = (signal: SInt) => signal + 1
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
      ret
    }
    TrinaryHardware(op, delay = (if (mreg) 1 else 0) cycles).asDeviceNode(s"macDSP48_${if (mreg) "usingMReg" else ""}")
  }

  // testing Systolic Array cell
  def SACell(width: BitCount): DeviceNode[SInt] = DeviceNode("SACell",
    DSPHardware(impl =
      (dataIns: Seq[SInt], _: GlobalCount) => {
        val result = dataIns(0) +^ dataIns(1)
        Seq(result, result)
      }, 2, Seq(width, 1 bits), 0 cycles, 0 sec)
  )

}
