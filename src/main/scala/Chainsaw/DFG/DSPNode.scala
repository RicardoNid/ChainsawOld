package Chainsaw.DFG

import spinal.core._

import scala.language.postfixOps

class DSPHardware[T <: Data](val impl: (Seq[T], GlobalCount) => Seq[T], val inDegree: Int, val outWidths: Seq[BitCount] = Seq(-1 bit)) {

  def asComponent(implicit holderProvider: BitCount => T) = () => new Component with NodeComponent[T] {
    override val dataIn: Vec[T] = in Vec(holderProvider(-1 bits), inDegree)
    override val dataOut: Vec[T] = out Vec(holderProvider(-1 bits), outWidths.size)
    dataOut := Vec(impl(dataIn, GlobalCount(U(0))))
  }

}

object DSPHardware {
  def apply[T <: Data](impl: (Seq[T], GlobalCount) => Seq[T], inDegree: Int, outWidths: Seq[BitCount]): DSPHardware[T] = new DSPHardware(impl, inDegree, outWidths)
}

class DSPNode[T <: Data](implp: DSPHardware[T], namep: String, delayp: CyclesCount, exeTimep: TimeNumber) {
  val hardware: DSPHardware[T] = implp
  val name: String = namep
  val delay: Int = delayp.toInt
  val exeTime: Double = exeTimep.toDouble

  def copy(newName: String): DSPNode[T] = new DSPNode(hardware, name, delay cycles, exeTime sec)

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

// TODO: better implementation of copy

class DeviceNode[T <: Data](implp: DSPHardware[T], namep: String, delayp: CyclesCount, exeTimep: TimeNumber)
  extends DSPNode(implp, namep, delayp, exeTimep) {
  override def copy(newName: String): DSPNode[T] = new DeviceNode(hardware, newName, delay cycles, exeTime sec)
}

object DeviceNode {
  def apply[T <: Data](hardware: DSPHardware[T], namep: String, delayp: CyclesCount, exeTimep: TimeNumber): DeviceNode[T] = new DeviceNode(hardware, namep, delayp, exeTimep)

  def apply[T <: Data](namep: String, delayp: CyclesCount, exeTimep: TimeNumber): DeviceNode[T] = new DeviceNode(Operators.passThrough[T](), namep, delayp, exeTimep)

  def apply[T <: Data](namep: String, delayp: CyclesCount, exeTimep: TimeNumber, width: BitCount): DeviceNode[T] = new DeviceNode(Operators.passThrough[T](width), namep, delayp, exeTimep)
}


class PassThrough[T <: Data](namep: String, width: BitCount = -1 bits) extends DSPNode[T](Operators.passThrough[T](width), namep, 0 cycles, 0 sec) {
  override def copy(newName: String): DSPNode[T] = new PassThrough[T](newName)
}

class InputNode[T <: Data](name: String) extends PassThrough[T](name) {
  override def copy(newName: String): InputNode[T] = new InputNode[T](newName)
}

object InputNode {
  def apply[T <: Data](name: String): InputNode[T] = new InputNode(name)
}

class OutputNode[T <: Data](name: String) extends PassThrough[T](name) {
  override def copy(newName: String): OutputNode[T] = new OutputNode[T](newName)
}

object OutputNode {
  def apply[T <: Data](name: String): OutputNode[T] = new OutputNode(name)
}

class VirtualNode[T <: Data](name: String, width: BitCount = -1 bits) extends PassThrough[T](name, width) {
  override def copy(newName: String): VirtualNode[T] = new VirtualNode[T](newName, width)
}

object VirtualNode {
  def apply[T <: Data](name: String, width: BitCount = -1 bits): VirtualNode[T] = new VirtualNode(name, width)
}

object ConstantHardware {
  def apply[THard <: Data, TSoft](constant: TSoft, width: BitCount)(implicit converter: (TSoft, BitCount) => THard): DSPHardware[THard] = DSPHardware(
    impl = (_: Seq[THard], _: GlobalCount) => Seq(converter(constant, width)),
    inDegree = 0,
    outWidths = Seq(width)
  )
}

class ConstantNode[T <: Data](namep: String, implp: DSPHardware[T])
  extends DSPNode[T](implp, namep, 0 cycles, 0 sec) {
  override def copy(newName: String): ConstantNode[T] = new ConstantNode(newName, hardware)
}

object ConstantNode {
  def apply[THard <: Data, TSoft](name: String, constant: TSoft, width: BitCount)
                                 (implicit converter: (TSoft, BitCount) => THard): ConstantNode[THard] = {
    val hardware = ConstantHardware(constant, width)
    new ConstantNode(s"constant_$constant", hardware)
  }

  def apply[THard <: Data, TSoft](name: String, hardType: HardType[THard], constant: TSoft)
                                 (implicit converter: (TSoft, BitCount) => THard): ConstantNode[THard] = {
    apply(name, constant, width = hardType.getBitsWidth bits) // FIXME: getBitsWidth is not available outside a Component
  }

  // TODO: this should be the constructor of ConstantNode
  def apply[T <: Data](namep: String, constant: T): ConstantNode[T] = new ConstantNode(namep, implp =
    DSPHardware(
      impl = (_: Seq[T], _: GlobalCount) => Seq(constant),
      inDegree = 0,
      outWidths = Seq(-1 bits)
    ))
}

// common nodes which we can build from the op directly
case class BinaryHardware[T <: Data](op: (T, T) => T, width: BitCount = -1 bits)
  extends DSPHardware[T](impl = (dataIns: Seq[T], _: GlobalCount) => Seq(op(dataIns(0), dataIns(1))), inDegree = 2, outWidths = Seq(width))

class BinaryNode[T <: Data](op: (T, T) => T, width: BitCount = -1 bits, name: String, delay: CyclesCount, exeTime: TimeNumber)
  extends DeviceNode[T](BinaryHardware(op, width), name, delay, exeTime) with Foldable[T] {
  override def copy(newName: String): BinaryNode[T] = new BinaryNode(op, width, newName, delay, exeTime)
}

object BinaryNode {
  def apply[T <: Data](op: (T, T) => T, name: String, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): BinaryNode[T] = new BinaryNode(op, width, name, delay, exeTime)
}

case class TrinaryHardware[T <: Data](op: (T, T, T) => T, width: BitCount = -1 bits)
  extends DSPHardware[T](impl = (dataIns: Seq[T], _: GlobalCount) => Seq(op(dataIns(0), dataIns(1), dataIns(2))), inDegree = 3, outWidths = Seq(width))

class TrinaryNode[T <: Data](op: (T, T, T) => T, width: BitCount = -1 bits, name: String, delay: CyclesCount, exeTime: TimeNumber)
  extends DeviceNode[T](TrinaryHardware(op, width), name, delay, exeTime) with Foldable[T] {
  override def copy(newName: String): TrinaryNode[T] = new TrinaryNode(op, width, newName, delay, exeTime)
}

object TrinaryNode {
  def apply[T <: Data](op: (T, T, T) => T, name: String, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): TrinaryNode[T] =
    new TrinaryNode(op, width, name, delay, exeTime)
}

/** Butterfly hardware takes two input and a coefficient, generates two output, they're heavily used as building blocks of more complicated DFG
 */
case class ButterflyHardware[THard <: Data, TSoft](op: (THard, THard, THard) => (THard, THard), width: BitCount = -1 bits) // (a, b, coeff)
  extends DSPHardware[THard](impl =
    (dataIns: Seq[THard], _: GlobalCount) => {
      val ret = op(dataIns(0), dataIns(1), dataIns(2))
      Seq(ret._1, ret._2)
    },
    inDegree = 3, outWidths = Seq(width, width))

class ButterflyNode[T <: Data](op: (T, T, T) => (T, T), width: BitCount = -1 bits, name: String, delay: CyclesCount, exeTime: TimeNumber)
  extends DeviceNode[T](ButterflyHardware(op, width), name, delay, exeTime) with Foldable[T] {
  override def copy(newName: String): ButterflyNode[T] = new ButterflyNode(op, width, newName, delay, exeTime)
}

object ButterflyNode {
  def apply[T <: Data](op: (T, T, T) => (T, T), name: String, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 1 ns): ButterflyNode[T] =
    new ButterflyNode(op, width, name, delay, exeTime)
}