package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class RxDataGen(implicit ftnParams: FtnParams) extends Component {

  import ftnParams.rxModulatedGolden

  val dataOut = master Stream Vec(SInt(6 bits), 128)

  val validLength = rxModulatedGolden.length
  val wholeLength = validLength + 8 // 72 / 80

  val counter = Counter(wholeLength, inc = dataOut.fire) // 72 / 80
  val valid = counter.value < U(validLength, counter.getWidth bits) // 72 / 80

  val rom: Mem[Vec[SInt]] = Mem(initialContent = rxModulatedGolden.map(vec => Vec(vec.map(S(_, 6 bits)))))

  dataOut.payload := Mux(valid, rom.readAsync(counter.value), Vec(dataOut.payload.map(_.getZero)))
  dataOut.valid := RegNext(valid, init = False)
}
