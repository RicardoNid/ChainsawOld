package Chainsaw.DFG

import spinal.core._

import scala.language.postfixOps

/* following contents are node wrappers which convert a simple op to a DSPNode with preset fields
  hardwares are declared to capture specific patterns, for example, a BinaryHardware always have a inDegree = 2 and a outDegree = 1
  nodes are declared to be used as annotations, and providing constructors

  to implement a new wrapper, search step1, 2, 3 in the source code

*/

// step1: declare a DSPHardware, capturing the characteristics by preset field
class BinaryHardware[T <: Data](op: (T, T) => T, width: BitCount = -1 bits, delay: Int, exeTime: Double)
  extends DSPHardware[T](
    impl = (dataIn: Seq[T], _: GlobalCount) => Seq(op(dataIn(0), dataIn(1))),
    inDegree = 2, outWidths = Seq(width), delay, exeTime)

class TrinaryHardware[T <: Data](op: (T, T, T) => T, width: BitCount = -1 bits, delay: Int, exeTime: Double)
  extends DSPHardware[T](
    impl = (dataIn: Seq[T], _: GlobalCount) => Seq(op(dataIn(0), dataIn(1), dataIn(2))),
    inDegree = 3, outWidths = Seq(width), delay, exeTime)

/**
 * @param op the order is a, b, coeff TODO: really?
 */
class ButterflyHardware[T <: Data](op: (T, T, T) => (T, T), width: BitCount = -1 bits, delay: Int, exeTime: Double)
  extends DSPHardware[T](
    impl = (dataIns: Seq[T], _: GlobalCount) => {
      val ret = op(dataIns(0), dataIns(1), dataIns(2))
      Seq(ret._1, ret._2)
    },
    inDegree = 3, outWidths = Seq(width, width), delay, exeTime)

// step2: declare a DSPNode extends the DeviceNode and override the copy method to avoid type regression
class BinaryNode[T <: Data](name: String, hardware: BinaryHardware[T]) extends DeviceNode(name, hardware) {
  override def copy(newName: String): BinaryNode[T] = new BinaryNode(name, hardware)
}

class TrinaryNode[T <: Data](name: String, hardware: TrinaryHardware[T]) extends DeviceNode(name, hardware) {
  override def copy(newName: String): TrinaryNode[T] = new TrinaryNode(name, hardware)
}

/** Butterfly hardware takes two input and a coefficient, generates two output, they're heavily used as building blocks of more complicated DFG, the two inputs are not symmetric
 */
class ButterflyNode[T <: Data](name: String, hardware: ButterflyHardware[T]) extends DeviceNode(name, hardware) {
  override def copy(newName: String): ButterflyNode[T] = new ButterflyNode(name, hardware)
}

// step3: declare a factory method as a entrance for the users
object BinaryNode {
  def apply[T <: Data](name: String, op: (T, T) => T,  width: BitCount = -1 bits,
                       delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 0 ns): BinaryNode[T] =
    new BinaryNode(name, new BinaryHardware(op, width, delay.toInt, exeTime.toDouble))
}

object TrinaryNode {
  def apply[T <: Data](name: String, op: (T, T, T) => T, width: BitCount = -1 bits,
                       delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 0 ns): TrinaryNode[T] =
    new TrinaryNode(name, new TrinaryHardware(op, width, delay.toInt, exeTime.toDouble))
}

object ButterflyNode {
  def apply[T <: Data](name: String, op: (T, T, T) => (T, T), width: BitCount = -1 bits, delay: CyclesCount = 0 cycles, exeTime: TimeNumber = 0 ns): ButterflyNode[T] =
    new ButterflyNode(name, new ButterflyHardware(op, width, delay.toInt, exeTime.toDouble))
}