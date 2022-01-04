package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class LoopBuffer[T <: Data]
(dataType: HardType[Vec[T]],
 loopLength: Int, frameLength: Int, iteration: Int)
  extends Component with DSPTestable[Vec[T], Vec[T]] {

  val start = in Bool() // for burst
  override val dataIn = slave Stream dataType
  override val dataOut = master Stream dataType
  override val latency = (iteration + 1) * loopLength

  require(loopLength % frameLength == 0)

  val iterationWidth = log2Up(iteration)
  val iterationType = HardType(UInt(iterationWidth bits))

  logger.info(s"buffer width: ${dataType().getBitsWidth}")
  val frameCounter = CounterFreeRun(frameLength)
  val loopCounter = CounterFreeRun(loopLength)

  // cyclic memory
  val dataHead = dataType()
  val dataBuffer = BigMem(dataType, loopLength)
  dataBuffer.write(loopCounter, dataHead)
  val dataTail = dataBuffer.readAsync(loopCounter.valueNext)

  val iterationHead = iterationType()
  val iterationBuffer = Mem(iterationType, loopLength)
  iterationBuffer.init(Seq.fill(loopLength)(U(0, iterationWidth bits)))
  iterationBuffer.write(loopCounter, iterationHead)
  val iterationTail = iterationBuffer.readAsync(loopCounter.valueNext)

  // conditions, caution! full and empty are "beyond current time" as we use them to drive getNew and getOld
  val empty = iterationTail === U(0)
  val full = iterationTail === U(iteration - 1)

  val getNew = RegNextWhen(start && empty, frameCounter.willOverflow, False)
  val getOld = RegNextWhen(!empty, frameCounter.willOverflow, False)
  val doNothing = !(getNew || getOld)

  // datapath
  dataHead := Mux(getNew, dataIn.payload, RegNext(dataTail))

  when(getNew)(iterationHead := U(1, iterationWidth bits))
    .elsewhen(getOld)(iterationHead := Mux(RegNext(full), U(0, iterationWidth bits), RegNext(iterationTail) + U(1, iterationWidth bits)))
    .otherwise(iterationHead := U(0, iterationWidth bits))

  // interface
  dataIn.ready := getNew
  dataOut.payload := RegNext(dataTail)
  dataOut.valid := RegNextWhen(True, frameCounter.willOverflow && start && empty, False)

  val iterLast = out(RegNextWhen(full, frameCounter.willOverflow, False)) // true "valid"
  val iterNew = out(iterationTail === U(1))
}
