package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class AsyncFIFOAnalysis[T <: Data](dataType: HardType[T], val depth: Int, val pushClock: ClockDomain, val popClock: ClockDomain) extends Component {

  assert(isPow2(depth) & depth >= 2, "The depth of the AsyncFIFOAnalysis must be a power of 2 and equal or bigger than 2")

  val io = new Bundle with StreamFifoInterface[T]{ // common interface for FIFO
    val push          = slave  Stream(dataType)
    val pop           = master Stream(dataType)
    val pushOccupancy = out UInt(log2Up(depth + 1) bits) // +1, as 0 to 2^k should be represented
    val popOccupancy  = out UInt(log2Up(depth + 1) bits)
  }

  val ptrWidth = log2Up(depth) + 1 // +1 using the extra MSB to distinguish full/empty
  // full logic: pointers are different at the MSB and same at other positions
  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1 downto ptrWidth - 2) === ~b(ptrWidth - 1 downto ptrWidth - 2) && a(ptrWidth - 3 downto 0) === b(ptrWidth - 3 downto 0)
  // empty logic: pointers are the same, meaning that the read pointer has caught up with the write pointer
  def isEmpty(a: Bits, b: Bits) = a === b

  val ram = Mem(dataType, depth) // memory

  val popToPushGray = Bits(ptrWidth bits) // clock-domain-cross(CDC) pointer, from pop to push
  val pushToPopGray = Bits(ptrWidth bits) // clock-domain-cross(CDC) pointer, from push to pop

  val pushCC = new ClockingArea(pushClock) {
    val pushPtr     = Reg(UInt(log2Up(2*depth) bits)) init(0) // push(write) pointer
    val pushPtrPlus = pushPtr + 1 // for logic
    val pushPtrGray = RegNextWhen(toGray(pushPtrPlus), io.push.fire) init(0)
    val popPtrGray  = BufferCC(popToPushGray, B(0, ptrWidth bits)) //
    val full        = isFull(pushPtrGray, popPtrGray) // the full logic is implemented in push clockDomain

    io.push.ready := !full

    when(io.push.fire) {
      ram(pushPtr.resized) := io.push.payload
      pushPtr := pushPtrPlus
    }

    io.pushOccupancy := (pushPtr - fromGray(popPtrGray)).resized
  }

  val popCC = new ClockingArea(popClock) {
    val popPtr      = Reg(UInt(log2Up(2*depth) bits)) init(0)
    val popPtrPlus  = popPtr + 1
    val popPtrGray  = RegNextWhen(toGray(popPtrPlus), io.pop.fire) init(0)
    val pushPtrGray = BufferCC(pushToPopGray, B(0, ptrWidth bit))
    val empty       = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid   := !empty
    io.pop.payload := ram.readSync((io.pop.fire ? popPtrPlus | popPtr).resized, clockCrossing = true)

    when(io.pop.fire) {
      popPtr := popPtrPlus
    }

    io.popOccupancy := (fromGray(pushPtrGray) - popPtr).resized
  }

  pushToPopGray := pushCC.pushPtrGray // the CDC assignment happens outside of the specific domain
  popToPushGray := popCC.popPtrGray

  val CC = Mem(UInt(4 bits),10)


}
