package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer

package object DFGNew {

  implicit val sintProvider = (width: BitCount) => if(width.value >= 1) SInt(width) else SInt()
  implicit val bitsProvider = (width: BitCount) => if(width.value >= 1) Bits(width) else Bits()

  implicit def Int2Unit(value: Int): IntUnitBuilder = new IntUnitBuilder(value)

  class IntUnitBuilder(val i: Int) extends AnyVal {
    def cycle = Cycle(i)
  }

  implicit class nodeUtils[T <: Data](node: DSPNode[T]) {
    def >=>(delay: Int) = DSPExpression(node, delay, node)

    def >>(delay: Int) = DSPPath(ArrayBuffer(node), ArrayBuffer(delay)) // TODO: more elegant implementation

    def -(that:DSPNode[T]) = DSPConstraint(node, that, 0)

    def isIO = node.isInstanceOf[InputNode[T]] || node.isInstanceOf[OutputNode[T]]
  }

  implicit class nodesUtils[T <: Data](nodes: Seq[DSPNode[T]]) {
    def >=>(delays: Seq[Int]) = DSPExpression(nodes, delays, nodes.head)
  }


}
