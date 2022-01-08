package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

case class TxDataGen() extends Component {

  val bits: Seq[BigInt] = loadFTN1d[Double]("txRaw").map(_.toInt).grouped(128).toSeq.map(bit128 => BigInt(bit128.mkString(""), 2))
  logger.info(s"dataGen period: ${bits.length}")
  val counter = CounterFreeRun(bits.length + 16)

  val rom = Mem(initialContent = bits.map(B(_, 128 bits)))
  val dataOut = master Stream Bits(128 bits)

  val valid = counter.value < U(bits.length, log2Up(bits.length + 16) bits)

  dataOut.payload := Mux(valid, rom.readSync(counter.value.takeLow(6).asUInt), dataOut.payload.getZero)
  dataOut.valid := valid
}
