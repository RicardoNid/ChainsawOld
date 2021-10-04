package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

package object DFG {

  implicit val sintProvider = (width: BitCount) => if(width.value >= 1) SInt(width) else SInt()
  implicit val uintProvider = (width: BitCount) => if(width.value >= 1) UInt(width) else UInt()
  implicit val bitsProvider = (width: BitCount) => if(width.value >= 1) Bits(width) else Bits()
  implicit val complexProvider = (width: BitCount) => ComplexNumber(1, width.value - 2)

  implicit class nodeUtils[T <: Data](node: DSPNode[T]) {
    def >=>(delay: Int) = DSPAssignment(node, delay, node)

    def >>(delay: Int) = DSPPath(ArrayBuffer(node), ArrayBuffer(delay)) // TODO: more elegant implementation

    def -(that:DSPNode[T]) = DSPConstraint(node, that, 0)

    def isIO = node.isInstanceOf[InputNode[T]] || node.isInstanceOf[OutputNode[T]]
  }

  implicit class nodesUtils[T <: Data](nodes: Seq[DSPNode[T]]) {
    def >=>(delays: Seq[Int]) = DSPAssignment(nodes, delays, nodes.head)
  }
}
