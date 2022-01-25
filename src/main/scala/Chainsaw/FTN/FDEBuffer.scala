package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class FDEBuffer[T <: Data]
(dataType: HardType[Vec[T]],
 loopLength: Int, frameLength: Int, iteration: Int)
  extends Component with DSPTestable[Vec[T], Vec[T]] {

  val start = in Bool() // for burst, this could be implemented by valid, but it change the semantic of valid, transfer would start 1 cycle after start
  override val dataIn = slave Stream dataType
  override val dataOut = master Stream dataType
  override val latency = loopLength

  require(loopLength % frameLength == 0)
  val iterationWidth = log2Up(iteration + 1)
  val iterationType = HardType(UInt(iterationWidth bits))

  // components
  val frameCounter = CounterFreeRun(frameLength) // for frame alignment
  val loopCounter = CounterFreeRun(loopLength)
  val dataBuffer = LoopMem(dataType, loopLength)
  val iterationBuffer = Mem(Seq.fill(loopLength)(U(0, iterationWidth bits)))

  // controls
  // conditions, caution! full and empty are "beyond current time" as we use them to drive getNew and getOld, when you use them directly, RegNext is needed
  val iterationTail = iterationBuffer.readAsync(loopCounter.valueNext)
  val empty = iterationTail === U(0) // current position is empty
  val full = iterationTail === U(iteration - 1) // last iteration

  val getNew = RegNextWhen(start && empty, frameCounter.willOverflow, False) // update when a new frame start
  val getOld = RegNextWhen(!empty, frameCounter.willOverflow, False)
  val doNothing = !(getNew || getOld)

  // data path
  val dataTail = dataType()
  dataBuffer.we := getNew
  dataBuffer.dataIn := dataIn.payload
  dataTail := dataBuffer.dataOut

  val iterationHead = iterationType()
  iterationBuffer.addAttribute("ram_style", "block")
  iterationBuffer.write(loopCounter, iterationHead)
  when(getNew)(iterationHead := U(1, iterationWidth bits))
    .elsewhen(getOld)(iterationHead := Mux(RegNext(full), U(0, iterationWidth bits), RegNext(iterationTail) + U(1, iterationWidth bits)))
    .otherwise(iterationHead := U(0, iterationWidth bits)) // do nothing

  // interface
  dataIn.ready := getNew
  dataOut.payload := dataTail
  dataOut.valid := RegNext(!empty, init = False)
  val iterLast = out(RegNext(full, init = False))
}
