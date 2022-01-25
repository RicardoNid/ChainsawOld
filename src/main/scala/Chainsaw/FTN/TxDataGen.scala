package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class TxDataGen(implicit ftnParams: FtnParams) extends Component {

  val frameLength = 64
  val period = frameLength + 16
  val bitsAllFrame = ftnParams.txBitsAll

  val innerCounter = CounterFreeRun(period) // 80, 7 bits
  val outerCounter = Counter(testSize, inc = innerCounter.willOverflow)

  val rom = Mem(initialContent = bitsAllFrame.map(B(_, 128 bits)))
  val dataOut = master Stream Bits(128 bits)

  val valid = RegNext(innerCounter.value < U(frameLength, log2Up(period) bits), init = False)
  dataOut.payload := Mux(valid, rom.readSync(outerCounter.value @@ innerCounter.value.takeLow(6).asUInt), dataOut.payload.getZero)
  dataOut.valid := valid

  //  val frameLength = 64
  //  val period = frameLength + 16
  //  val bits: Seq[BigInt] = ftnParams.txBitsAll
  //  logger.info(s"dataGen period: ${bits.length}")f
  //  val counter = CounterFreeRun(period) // 80, 7 bits
  //
  //  val rom = Mem(initialContent = bits.map(B(_, 128 bits)))
  //  val dataOut = master Stream Bits(128 bits)
  //
  //  val valid = RegNext(counter.value < U(bits.length, log2Up(period) bits), init = False)
  //  dataOut.payload := Mux(valid, rom.readSync(counter.value.takeLow(6).asUInt), dataOut.payload.getZero)
  //  dataOut.valid := valid
}
