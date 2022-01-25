package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class RxOnBoard(actual: Int)(implicit ftnParams: FtnParams) extends Component {

  val dataOut = master Stream Bits(512 bits)

  val rxDataGen = RxDataGen()
  val rx = RxFull(actual, 5)

  rxDataGen.dataOut >> rx.dataIn
  rx.dataOut.payloadMap(_ => Mux(rx.dataOut.valid, rx.dataOut.payload, Bits(512 bits).getZero)) >> dataOut
}

object RxOnBoard extends App {

  implicit val ftnParams = FtnParams(3,226,doBitAlloc = true)
  SimConfig.withFstWave.compile(RxOnBoard(16)).doSim { dut =>

    dut.clockDomain.forkStimulus(2)
    dut.clockDomain.waitSampling(10000)
  }
}
