package Chainsaw.comm.viterbi

import Chainsaw.dspTest._
import breeze.numerics.nextPower2
import spinal.core._
import spinal.lib._

case class DataReverse[T <: Data](dataType: HardType[T], length: Int, readAsync: Boolean = true)
  extends Component with DSPTestable[T, T] {

  override val dataIn = slave Stream dataType()
  override val dataOut = master Stream dataType()
  override val latency = length

  val lengthUpper = nextPower2(length).toInt
  val ram = Mem(dataType(), lengthUpper << 1)
  if(!readAsync) ram.addAttribute("ram_style", "block")

  // TODO: eliminate one counter
  val counterWrite = Counter(length)
  val counterRead = Counter(length)
  val doWrite = dataIn.fire || counterWrite =/= U(0)
  val writeFinish = RegNext(counterWrite.willOverflow, init = False)
  val doRead = writeFinish || counterRead =/= U(0)

  dataIn.ready := True
  when(doWrite)(counterWrite.increment())
  when(doRead)(counterRead.increment())
  if (readAsync) dataOut.valid := doRead
  else dataOut.valid := RegNext(doRead, init = False)

  val pingPongPointer = RegInit(False)
  when(counterWrite.willOverflow)(pingPongPointer := ~pingPongPointer)

  val writeAddr: UInt = pingPongPointer.asUInt @@ counterWrite.value
  val readAddr: UInt = ~pingPongPointer.asUInt @@ (U(length - 1) - counterRead)

  ram.write(writeAddr, dataIn.payload, doWrite)
  if (readAsync) dataOut.payload := ram.readAsync(readAddr)
  else dataOut.payload := ram.readSync(readAddr)
}
