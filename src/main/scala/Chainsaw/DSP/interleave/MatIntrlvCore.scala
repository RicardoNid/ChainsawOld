package Chainsaw.DSP.interleave

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class MatIntrlvCore[T <: Data](row: Int, col: Int, dataType: HardType[T]) extends Component {

  val dataIn = slave Stream Vec(dataType(), col)
  val dataOut = master Stream Vec(dataType(), row)

  val ramCount, ramDepth = row max col
  val addrWidth = log2Up(ramDepth)

  // ping-pong was implemented by using different address range of the same dual-port ram
  val rams = Seq.fill(ramCount)(Mem(dataType, 1 << (addrWidth + 1)))

  // 0 for ping(lower addr), 1 for pong(higher addr)
  val readPointer = RegInit(False)
  val writePointer = RegInit(False)

  val counterIn = Counter(row)
  val counterOut = Counter(col)

  // padding are used to simplify rotation logics, they will be pruned by synthsizer

  // write ports
  val dataInPadded = Vec(dataIn.payload.padTo(ramCount, dataIn.payload(0).getZero))
  val dataInShifted: Vec[T] = dataInPadded.rotateLeft(counterIn.value)
  rams.zip(dataInShifted).foreach { case (ram, data) =>
    ram.write(address = writePointer.asUInt @@ counterIn.value.resize(addrWidth), data = data, enable = dataIn.fire)
  }

  // read address generator
  val readAddrs = Vec(UInt(addrWidth bits), ramCount)
  readAddrs := Vec((0 +: (1 until ramCount).reverse).map(i => U(i, addrWidth bits)))
    .rotateRight(counterOut.value)

  // read ports
  val dataOutShifted: Vec[T] = Vec(rams.zip(readAddrs).map { case (ram, addr) =>
    ram.readAsync(address = readPointer.asUInt @@ addr)
  }.padTo(ramCount, dataIn.payload(0).getZero))
  val dataRemapped = Vec(dataOutShifted.head +: dataOutShifted.tail.reverse)
  val dataOutPadded = dataRemapped.rotateRight(counterOut.value) // not shifted now, still padded
  dataOut.payload.zip(dataOutPadded).foreach { case (out, padded) => out := padded } // drop the padded part by zip mechanism

  val fsm = new StateMachine {

    when(dataIn.fire)(counterIn.increment())
    when(dataOut.fire)(counterOut.increment())

    when(counterIn.willOverflow)(writePointer := ~writePointer)
    when(counterOut.willOverflow)(readPointer := ~readPointer)

    val EMPTY = StateEntryPoint()
    val HALF, FULL = State()

    EMPTY.whenIsActive(when(counterIn.willOverflow)(goto(HALF)))
    FULL.whenIsActive(when(counterOut.willOverflow)(goto(HALF)))
    HALF.whenIsActive {
      when(counterIn.willOverflow && counterOut.willOverflow)(goto(HALF))
        .elsewhen(counterIn.willOverflow)(goto(FULL))
        .elsewhen(counterOut.willOverflow)(goto(EMPTY))
    }

    dataIn.ready := isActive(EMPTY) || isActive(HALF)
    dataOut.valid := isActive(FULL) || isActive(HALF)
  }


}

object MatIntrlvCore extends App {
  val dataType = HardType(UInt(4 bits))
  VivadoSynth(new MatIntrlvCore(32, 32, dataType))
  //  VivadoSynth(new MatIntrlvCore(10, 10, dataType))
}
