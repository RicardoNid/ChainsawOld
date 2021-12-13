package Chainsaw.DFG

import Chainsaw._
import spinal.core._

import scala.collection.immutable
import scala.language.postfixOps

/** hardware device of a DSPNode
 *
 * @param impl      the way it works described in SpinalHDL
 * @param inDegree  input port number
 * @param outWidths output port number
 * @tparam T hardware signal type in SpinalHDL
 */
class DSPHardware[T <: Data](val impl: (Seq[T], GlobalCount) => Seq[T],
                             val inDegree: Int, val outWidths: Seq[BitCount],
                             val delay: Int, val exeTime: Double) {

  def asComponent(namep: String)(implicit holderProvider: HolderProvider[T]): () => Component with NodeComponent[T] = () => new Component with NodeComponent[T] {
    setDefinitionName(namep)
    override val dataIn: Vec[T] = in Vec(holderProvider(-1 bits), inDegree)
    override val dataOut: Vec[T] = out Vec(holderProvider(-1 bits), outWidths.size)
    dataOut := Vec(impl(dataIn, GlobalCount(U(0))))
  }

  def asDeviceNode(name: String): DeviceNode[T] = DeviceNode(name, this)


  def asDeviceNodes(name: String, count: Int): Seq[DeviceNode[T]]
  = (0 until count).map(i => asDeviceNode(s"${name}_$i"))

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
  def apply[T <: Data](impl: (Seq[T], GlobalCount) => Seq[T], inDegree: Int, outWidths: Seq[BitCount],
                       delay: CyclesCount, exeTime: TimeNumber): DSPHardware[T] =
    new DSPHardware(impl, inDegree, outWidths, delay.toInt, exeTime.toDouble)
}

// TODO: better implementation of copy
class DSPNode[T <: Data](val name: String, val hardware: DSPHardware[T]) {

  def impl: (Seq[T], GlobalCount) => Seq[T] = hardware.impl

  def inDegree: Int = hardware.inDegree

  def outDegree: Int = hardware.outWidths.size

  def outWidths: Seq[BitCount] = hardware.outWidths

  def delay: Int = hardware.delay

  def exeTime: Double = hardware.exeTime

  def copy(newName: String): DSPNode[T] = new DSPNode(newName, hardware)

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
class DeviceNode[T <: Data](override val name: String,
                            override val hardware: DSPHardware[T])
  extends DSPNode(name, hardware) {
  override def copy(newName: String): DSPNode[T] = new DeviceNode[T](newName, hardware)
}

object DeviceNode {
  def apply[T <: Data](name: String, hardware: DSPHardware[T]): DeviceNode[T] =
    new DeviceNode(name, hardware)
}

/** a node acting as a pass through, which means it has no delay or exeTime, and pass the input to the output
 */
class PassThrough[T <: Data](override val name: String, width: BitCount = -1 bits)
  extends DSPNode[T](name, Operators.passThrough[T](width))

// the following classes have no difference or preset field compared to the PassThrough
// we declare them as annotations of different roles a node can play in DFG
class InputNode[T <: Data](name: String, width: BitCount = -1 bits) extends PassThrough[T](name, width) {
  override def copy(newName: String): InputNode[T] = new InputNode(newName, width)
}

class OutputNode[T <: Data](name: String, width: BitCount = -1 bits) extends PassThrough[T](name, width) {
  override def copy(newName: String): OutputNode[T] = new OutputNode(newName, width)
}

class VirtualNode[T <: Data](name: String, width: BitCount = -1 bits) extends PassThrough[T](name, width) {
  override def copy(newName: String): VirtualNode[T] = new VirtualNode(newName, width)
}

object InputNode {
  def apply[T <: Data](name: String, width: BitCount = -1 bits): InputNode[T] = new InputNode(name)
}

object OutputNode {
  def apply[T <: Data](name: String, width: BitCount = -1 bits): OutputNode[T] = new OutputNode(name)
}

object VirtualNode {
  def apply[T <: Data](name: String, width: BitCount = -1 bits): VirtualNode[T] = new VirtualNode(name, width)
}

/** using "hard" value directly, although seems to be natural, this won't work! for the reason
 *
 * @see [[examples.ContextProblemExample]]
 */
object ConstantHardware {
  def apply[THard <: Data, TSoft](constant: TSoft, width: BitCount)
                                 (implicit converter: (TSoft, BitCount) => THard): DSPHardware[THard] = DSPHardware(
    // this style delay the hardware literal generation until impl process
    impl = (_: Seq[THard], _: GlobalCount) => Seq(converter(constant, width)),
    inDegree = 0,
    outWidths = Seq(width),
    delay = 0 cycles, exeTime = 0 sec
  )

  def apply[THard <: Data](constant: THard): DSPHardware[THard] = DSPHardware(
    // this style delay the hardware literal generation until impl process
    impl = (_: Seq[THard], _: GlobalCount) => Seq(constant),
    inDegree = 0,
    outWidths = Seq(constant.getBitsWidth bits),
    delay = 0 cycles, exeTime = 0 sec
  )
}

/** nodes which represent a constant(literal in verilog) in the DFG, for its usage, view its factory methods, instead of its constructor
 */
class ConstantNode[T <: Data](name: String, hardware: DSPHardware[T]) extends DSPNode[T](name, hardware) {
  override def copy(newName: String): DSPNode[T] = new ConstantNode(name, hardware)

  def getConstant: T = impl(null, null).head
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

  def apply[THard <: Data](name: String, constant: THard): ConstantNode[THard] = {
    val hardware = ConstantHardware(constant)
    new ConstantNode(name, hardware)
  }

}