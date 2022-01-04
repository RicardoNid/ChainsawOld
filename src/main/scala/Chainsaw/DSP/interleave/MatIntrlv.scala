package Chainsaw.DSP.interleave

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

/** implement the matrix interleave function in a parallel way, data input row by row and output column by column
 *
 * @param row      number of rows, it is also the parallelism of output
 * @param col      number of columns, it is also the parallelism of output
 * @param dataType hard type of an 'element'
 * @see ''Matlab doc: matintrlv''
 */
case class MatIntrlv[T <: Data](row: Int, col: Int, dataType: HardType[T])
  extends Component with DSPTestable[Vec[T], Vec[T]] {

  override val dataIn: Stream[Vec[T]] = slave Stream Vec(dataType(), col)
  override val dataOut: Stream[Vec[T]] = master Stream Vec(dataType(), row)
  override val latency = row // caution: the valid input length may differ from the valid output, latency means the gap between their heads
  logger.info(s"implementing a $row * $col MatIntrlv of ${dataType.getBitsWidth}-bits-width elements, latency = $latency")

  // we need a 'square' matrix to contain all data, when row != col, there exists redundant space
  // besides, for depth, we use a power of 2, which will introduce redundant space sometimes, but
  // it is suitable for FPGAs
  // it simplifies the control logic
  val ramCount = row max col
  val addrWidth: Int = log2Up(row max col)
  val ramDepth = 1 << addrWidth
  // ping-pong was implemented by using different address range of the same dual-port ram
  val rams: Seq[Mem[T]] = Seq.fill(ramCount)(Mem(dataType(), ramDepth << 1))

  // 0 for ping(lower addr), 1 for pong(higher addr)
  val readPointer: Bool = RegInit(False)
  val writePointer: Bool = RegInit(False)

  // counters which are driven by fires, they control the rotation of data / generation of address
  val counterIn: Counter = Counter(row)
  val counterOut: Counter = Counter(col)

  // the datapath:
  // dataIn:  in -> padded -> shifted -> write in natural addrs -> mem
  // dataOut: mem -> read by generated addrs -> shifted -> (remapped) -> padded -> out

  // write logic
  // while writing data, we rotate the input vector to match the ports

  // padding are used to simplify rotation logics, they will be pruned by synthesizer
  val zero = dataType().getZero
  val dataInPadded: Vec[T] = Vec(dataIn.payload.padTo(ramCount, zero))
  val dataInShifted: Vec[T] = cloneOf(dataInPadded)

  // as rotate left of vec is not implemented by SpinalHDL, we implement this
  def connectRotateLeft(i: Int): Unit = {
    val shifted = dataInPadded.takeRight(ramCount - i) ++ dataInPadded.take(i)
    dataInShifted := Vec(shifted)
  }

  switch(counterIn.value) {
    (0 until row).foreach(i => is(U(i))(connectRotateLeft(i))) // rotate to match the ports
    if (!isPow2(row)) default(dataInShifted.assignDontCare()) // when default is needed
  }

  // write ports
  rams.zip(dataInShifted).foreach { case (ram, data) =>
    ram.write(
      address = writePointer.asUInt @@ counterIn.value.resize(addrWidth), // using write pointer as the MSB
      data = data, enable = dataIn.fire)
  }

  // read logic
  // while reading data, we rotate the addresses to get proper address

  // read address generation
  val initValues = Vec((0 +: (1 until ramCount).reverse).map(U(_, addrWidth bits)))
  val readAddrs = RegInit(initValues)
  when(dataOut.fire){
    when(counterOut.willOverflow)(readAddrs := initValues)
      .otherwise(readAddrs := readAddrs.rotateRight(1))
  }

  // read ports
  val dataOutShifted: Vec[T] = Vec(
    rams.zip(readAddrs).map { case (ram, addr) => // zip ports with addrs
      ram.readAsync(
        address = readPointer.asUInt @@ addr,
        readUnderWrite = writeFirst) // using read pointer as MSB
    }.padTo(ramCount, zero))
  val dataRemapped = Vec(dataOutShifted.head +: dataOutShifted.tail.reverse)
  val dataOutPadded = cloneOf(dataRemapped)

  // reverse of RotateLeft
  def connectRotateRight(i: Int): Unit = {
    val shifted = dataRemapped.takeRight(i) ++ dataRemapped.take(ramCount - i)
    dataOutPadded := Vec(shifted)
  }

  // while reading data, we still need to rotate the output vector to match the ports
  switch(counterOut.value) {
    (0 until col).foreach(i => is(U(i))(connectRotateRight(i)))
    if (!isPow2(col)) default(dataOutPadded.assignDontCare())
  }

  dataOut.payload := Vec(dataOutPadded.take(row)) // drop the padded part

  // ping-pong state machine
  val fsm: StateMachine = new StateMachine {

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