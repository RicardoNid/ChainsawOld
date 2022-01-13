package Chainsaw

import breeze.numerics._
import spinal.core._
import spinal.lib._

case class BigStreamFifo[T <: Data](dataType: HardType[Vec[T]], depth: Int)
  extends Component {

  val io = new Bundle {
    val push = slave Stream dataType
    val pop = master Stream dataType
    val flush = in Bool() default False
    val occupancy = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }

  val bitWidth = dataType().getBitsWidth
  val vecWidth = dataType().length
  val subFifoNum = nextPower2(ceil(bitWidth / 4096.0)).toInt
  require(vecWidth % subFifoNum == 0, s"vec width $vecWidth, sub fifo number $subFifoNum")
  val subVecWidth = vecWidth / subFifoNum

  logger.info(s"your fifo is divided into $subFifoNum sub fifos to avoid way too big signal")

  val elementType = HardType(dataType().head)

  val subFifos = Seq.fill(subFifoNum)(StreamFifo(Vec(elementType, vecWidth / subFifoNum), depth))

  def vecSlice(in: Vec[T], index: Int) = Vec(in.slice(index * subVecWidth, (index + 1) * subVecWidth))

  io.push.ready.allowOverride
  subFifos.zipWithIndex.foreach { case (fifo, i) =>
    io.push.payloadMap(vecSlice(_, i)) >> fifo.io.push
    vecSlice(io.pop.payload, i) := fifo.io.pop.payload
    fifo.io.pop.ready := io.pop.ready
    fifo.io.flush := io.flush
  }
  io.pop.valid := subFifos.head.io.pop.valid
  io.occupancy := subFifos.head.io.occupancy
  io.availability := subFifos.head.io.availability
}
