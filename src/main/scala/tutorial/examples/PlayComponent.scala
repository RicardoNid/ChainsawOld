package tutorial.examples

import spinal.core.{UInt, _}


class PlayComponent extends Component {

  implicit class myBitVector(val bv: BitVector) {
    /** Allocate
     */
    def =<<[T <: BitVector](elems: T*) = {
      val bw = bv.getBitsWidth
      val prefix = (0 to elems.length).map(i => elems.map(_.getBitsWidth).take(i).sum)
      val intervals = prefix.dropRight(1).zip(prefix.drop(1))
      val slices = intervals.map { case (start, end) => bv.asBits(bw - 1 - start downto bw - end) }
      elems.zip(slices).foreach { case (elem, slice) => elem.assignFromBits(slice) }
    }
  }

  val a = UInt(2 bits)
  val b = UInt(4 bits)
  val c = UInt(2 bits)
  val d = UInt(3 bits)
  val e = UInt(5 bits)

  //  def allocate[T <: BitVector](data: T, elems: T*) = {
  //    val bw = data.getBitsWidth
  //    val prefix = (0 to elems.length).map(i => elems.map(_.getBitsWidth).take(i).sum)
  //    val intervals = prefix.dropRight(1).zip(prefix.drop(1))
  //    val slices = intervals.map { case (start, end) => data.asBits(bw - 1 - start downto bw - end) }
  //    elems.zip(slices).foreach { case (elem, slice) => elem.assignFromBits(slice) }
  //  }

  (d ## e) =<< (a, b, c)

  in(d, e)
  out(a, b, c)
}

object PlayComponent {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new PlayComponent)
  }
}
