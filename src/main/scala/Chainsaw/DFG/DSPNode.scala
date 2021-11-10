package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

/** hardware device of a DSPNode
 *
 * @param impl      the way it works described in SpinalHDL
 * @param inDegree  input port number
 * @param outWidths output port number
 * @tparam T hardware signal type in SpinalHDL
 */
class DSPHardware[T <: Data](val impl: (Seq[T], GlobalCount) => Seq[T],
                             val inDegree: Int, val outWidths: Seq[BitCount] = Seq(-1 bit)) {

  def asComponent(namep: String)(implicit holderProvider: HolderProvider[T]): () => Component with NodeComponent[T] = () => new Component with NodeComponent[T] {
    setDefinitionName(namep)
    override val dataIn: Vec[T] = in Vec(holderProvider(-1 bits), inDegree)
    override val dataOut: Vec[T] = out Vec(holderProvider(-1 bits), outWidths.size)
    dataOut := Vec(impl(dataIn, GlobalCount(U(0))))
  }

  //  def inferable(implicit holderProvider: HolderProvider[T]): Boolean = {
  //    GenRTL(new Component {
  //      val core = asComponent(holderProvider)()
  //      val dataIn: Vec[T] = in Vec(holderProvider(5 bits), inDegree)
  //      val dataOut: Vec[T] = out Vec(holderProvider(-1 bits), outWidths.size)
  //
  //      core.dataIn := dataIn
  //      dataOut := core.dataOut
  //    })
  //  }
}

object DSPHardware {
  def apply[T <: Data](impl: (Seq[T], GlobalCount) => Seq[T], inDegree: Int, outWidths: Seq[BitCount]): DSPHardware[T] = new DSPHardware(impl, inDegree, outWidths)
}

// TODO: better implementation of copy
class DSPNode[T <: Data](
                          val hardware: DSPHardware[T], val name: String,
                          val delay: Int, val exeTime: Double) {
  def copy(newName: String): DSPNode[T] = new DSPNode(hardware, name, delay, exeTime)

  override def toString: String = name

  // classification of nodes
  def isConstant: Boolean = this.isInstanceOf[ConstantNode[T]]

  def isInput: Boolean = this.isInstanceOf[InputNode[T]]

  def isOutput: Boolean = this.isInstanceOf[OutputNode[T]]

  /** inner nodes will be implemented as concrete hardware, while I/O and constant nodes will not be
   */
  def isInner: Boolean = !isOutput && !isInput && !isConstant

  def isOuter: Boolean = !isInner

  def apply(order: Int): DSPNodeWithOrder[T] = DSPNodeWithOrder(this, order)

}

/** nodes that need to be actually implemented in a DFG, it does computation
 *
 * it has no difference or preset field compared to the general DFGNode, we declare this as an annotation of hardware device
 */
class DeviceNode[T <: Data](
                             override val hardware: DSPHardware[T], override val name: String,
                             override val delay: Int, override val exeTime: Double)
  extends DSPNode(hardware, name, delay, exeTime) {
}

object DeviceNode {
  def apply[T <: Data](hardware: DSPHardware[T], name: String, delay: CyclesCount, exeTime: TimeNumber): DeviceNode[T] =
    new DeviceNode(hardware, name, delay.toInt, exeTime.toDouble)
}


/** a node acting as a pass through, which means it has no delay or exeTime, and pass the input to the output
 */
class PassThrough[T <: Data](override val name: String, width: BitCount = -1 bits)
  extends DSPNode[T](Operators.passThrough[T](width), name, 0, 0) {
  override def copy(newName: String): DSPNode[T] = new PassThrough[T](newName)
}

// the following classes have no difference or preset field compared to the PassThrough
// we declare them as annotations of different roles a node can play in DFG
class InputNode[T <: Data](name: String, width: BitCount = -1 bits) extends PassThrough[T](name, width) {
  override def copy(newName: String): InputNode[T] = new InputNode[T](newName)
}

object InputNode {
  def apply[T <: Data](name: String, width: BitCount = -1 bits): InputNode[T] = new InputNode(name)
}

class OutputNode[T <: Data](name: String, width: BitCount = -1 bits) extends PassThrough[T](name, width) {
  override def copy(newName: String): OutputNode[T] = new OutputNode[T](newName)
}

object OutputNode {
  def apply[T <: Data](name: String, width: BitCount = -1 bits): OutputNode[T] = new OutputNode(name)
}

class VirtualNode[T <: Data](name: String, width: BitCount = -1 bits) extends PassThrough[T](name, width) {
  override def copy(newName: String): VirtualNode[T] = new VirtualNode[T](newName, width)
}

object VirtualNode {
  def apply[T <: Data](name: String, width: BitCount = -1 bits): VirtualNode[T] = new VirtualNode(name, width)
}

/** nodes which represent a constant(literal in verilog) in the DFG, for its usage, view its factory methods, instead of its constructor
 */
class ConstantNode[T <: Data](name: String, hardware: DSPHardware[T])
  extends DSPNode[T](hardware, name, 0, 0) {
  override def copy(newName: String): ConstantNode[T] = new ConstantNode(newName, hardware)
}

object ConstantHardware {
  def apply[THard <: Data, TSoft](constant: TSoft, width: BitCount)
                                 (implicit converter: (TSoft, BitCount) => THard): DSPHardware[THard] = DSPHardware(
    // this style delay the hardware literal generation until impl process
    impl = (_: Seq[THard], _: GlobalCount) => Seq(converter(constant, width)),
    inDegree = 0,
    outWidths = Seq(width)
  )
}

/** different entrance of creating constant nodes, common converters are provided in the package object of [[DFG]]
 *
 */
object ConstantNode {

  // value + width + converter
  def apply[THard <: Data, TSoft](name: String, constant: TSoft, width: BitCount)
                                 (implicit converter: (TSoft, BitCount) => THard): ConstantNode[THard] = {
    val hardware = ConstantHardware(constant, width)
    new ConstantNode(s"constant_$constant", hardware)
  }

  // value + hard type + converter
  def apply[THard <: Data, TSoft](name: String, hardType: HardType[THard], constant: TSoft)
                                 (implicit converter: (TSoft, BitCount) => THard): ConstantNode[THard] = {
    apply(name, constant, width = hardType.getBitsWidth bits) // FIXME: getBitsWidth is not available outside a Component
  }

  /** using "hard" value directly, although seems to be natural, this won't work! for the reason
   *
   * @see [[examples.ContextProblemExample]]
   */
  @deprecated
  def apply[T <: Data](name: String, constant: T): ConstantNode[T] = new ConstantNode(name, hardware =
    DSPHardware(
      impl = (_: Seq[T], _: GlobalCount) => Seq(constant),
      inDegree = 0,
      outWidths = Seq(-1 bits)
    ))
}