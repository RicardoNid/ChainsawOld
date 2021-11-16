package Chainsaw.DFG

import spinal.core._

import scala.language.postfixOps

/** following contents are node wrappers which convert a simple op to a DSPNode with preset fields
 *
 * hardwares are declared to capture specific patterns, for example, a BinaryHardware always have a inDegree = 2 and a outDegree = 1
 *
 * nodes are declared to be used as annotations, so that different patterns can be recognized by type system
 *
 * to implement a new wrapper, search step1, 2, 3 in the source code
 */

// step1: declare a DSPNode extends the DeviceNode and override the copy method to avoid type regression
// the subclasses of DSPNode capture no patterns, just acting as annotations

class UnaryNode[T <: Data](name: String, hardware: UnaryHardware[T]) extends DeviceNode(name, hardware) {
  override def copy(newName: String): UnaryNode[T] = new UnaryNode(newName, hardware)
}

class BinaryNode[T <: Data](name: String, hardware: BinaryHardware[T]) extends DeviceNode(name, hardware) {
  override def copy(newName: String): BinaryNode[T] = new BinaryNode(newName, hardware)
}

class TrinaryNode[T <: Data](name: String, hardware: TrinaryHardware[T]) extends DeviceNode(name, hardware) {
  override def copy(newName: String): TrinaryNode[T] = new TrinaryNode(newName, hardware)
}

class ButterflyNode[T <: Data](name: String, hardware: ButterflyHardware[T]) extends DeviceNode(name, hardware) {
  override def copy(newName: String): ButterflyNode[T] = new ButterflyNode(newName, hardware)
}

// step2: declare a DSPHardware, capturing the characteristics by preset field
class UnaryHardware[T <: Data](op: T => T, width: BitCount, delay: Int, exeTime: Double)
  extends DSPHardware[T](
    impl = (dataIn: Seq[T], _: GlobalCount) => Seq(op(dataIn(0))),
    inDegree = 1, outWidths = Seq(width), delay, exeTime) {
  override def asDeviceNode(name: String): UnaryNode[T] = new UnaryNode[T](name, this)
}

object UnaryHardware {
  def apply[T <: Data](op: T => T, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 0 sec): UnaryHardware[T] =
    new UnaryHardware(op, width, delay.toInt, exeTime.toDouble)
}

class BinaryHardware[T <: Data](op: (T, T) => T, width: BitCount, delay: Int, exeTime: Double)
  extends DSPHardware[T](
    impl = (dataIn: Seq[T], _: GlobalCount) => Seq(op(dataIn(0), dataIn(1))),
    inDegree = 2, outWidths = Seq(width), delay, exeTime) {
  override def asDeviceNode(name: String): BinaryNode[T] = new BinaryNode(name, this)
}

object BinaryHardware {
  def apply[T <: Data](op: (T, T) => T, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 0 sec): BinaryHardware[T] =
    new BinaryHardware(op, width, delay.toInt, exeTime.toDouble)
}


class TrinaryHardware[T <: Data](op: (T, T, T) => T, width: BitCount, delay: Int, exeTime: Double)
  extends DSPHardware[T](
    impl = (dataIn: Seq[T], _: GlobalCount) => Seq(op(dataIn(0), dataIn(1), dataIn(2))),
    inDegree = 3, outWidths = Seq(width), delay, exeTime) {
  override def asDeviceNode(name: String): TrinaryNode[T] = new TrinaryNode[T](name, this)
}

object TrinaryHardware {
  def apply[T <: Data](op: (T, T, T) => T, width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 0 sec): TrinaryHardware[T] =
    new TrinaryHardware[T](op, width, delay.toInt, exeTime.toDouble)
}

/** butterfly hardware takes two input and a coefficient, generates two output, they're heavily used as building blocks of more complicated DFG, the two inputs are not symmetric
 *
 * @param op the order is a, b, coeff
 *           TODO: really?
 */
class ButterflyHardware[T <: Data](op: (T, T, T) => (T, T), width: BitCount, delay: Int, exeTime: Double)
  extends DSPHardware[T](
    impl = (dataIns: Seq[T], _: GlobalCount) => {
      val ret = op(dataIns(0), dataIns(1), dataIns(2))
      Seq(ret._1, ret._2)
    },
    inDegree = 3, outWidths = Seq(width, width), delay, exeTime) {
  override def asDeviceNode(name: String): ButterflyNode[T] = new ButterflyNode[T](name, this)
}

object ButterflyHardware {
  def apply[T <: Data](op: (T, T, T) => (T, T), width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 0 sec): ButterflyHardware[T] =
    new ButterflyHardware[T](op, width, delay.toInt, exeTime.toDouble)
}