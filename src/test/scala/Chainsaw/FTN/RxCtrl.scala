package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class SplitterCtrl() extends Component {

  import RxConfig._

  val dataIn = slave Stream testType()
  val dataOut, preambleOut = master Stream testType()

  val counter = Counter(18, inc = dataIn.fire)
  dataIn.ready := (preambleOut.ready && counter < 2) || (dataOut.ready && counter >= 2)

  dataOut.payload := dataIn.payload
  dataOut.valid := (counter >= 2 && dataIn.valid)

  preambleOut.payload := dataIn.payload
  preambleOut.valid := (counter < 2 && dataIn.valid)
}

case class SmoothCtrl() extends Component {

  import RxConfig._

  val preambleIn = slave Stream testType()
  val preambleOut = master Stream testType()
  preambleIn.ready := False

  val fsm = new StateMachine {

    val batchCounter = Counter(smoothBatch, inc = preambleIn.fire)
    val RECEIVE = StateEntryPoint()
    val PROCESS = new StateDelay(smoothLatency - smoothBatch)

    RECEIVE.whenIsActive {
      when(batchCounter.willOverflow)(goto(PROCESS))
      preambleIn.ready := True
    }

    val last = Bool()
    last := False
    PROCESS.whenCompleted {
      goto(RECEIVE)
      last := True
    }

    preambleOut.valid := RegNext(last)
    preambleOut.payload := Delay(preambleIn.payload, smoothLatency)
  }
}

case class EqualCtrl() extends Component {

  import RxConfig._

  val preambleIn, dataIn = slave Stream testType()
  val dataOut = master Stream testType()

  preambleIn.ready := False
  dataIn.ready := False

  val fsm = new StateMachine {

    val batchCounter0 = Counter(equalBatch0, inc = preambleIn.fire)
    val batchCounter1 = Counter(equalBatch1, inc = dataIn.fire)
    val RECEIVE0 = StateEntryPoint()
    val RECEIVE1 = State()
    val PROCESS0 = new StateDelay(equalLatency0 - equalBatch0)
    val PROCESS1 = new StateDelay(equalLatency1 - equalBatch1)


    RECEIVE0.whenIsActive {
      when(batchCounter0.willOverflow)(goto(PROCESS0))
      preambleIn.ready := True
    }

    PROCESS0.whenCompleted(goto(RECEIVE1))

    RECEIVE1.whenIsActive {
      when(batchCounter1.willOverflow)(goto(PROCESS1))
      dataIn.ready := True
    }

    val trigger = False
    val triggered = Trigger(trigger, equalBatch1)
    PROCESS1.whenCompleted {
      goto(RECEIVE0)
      trigger := True
    }

    dataOut.valid := triggered
    dataOut.payload := Delay(dataIn.payload, equalLatency1)
  }
}

case class EqualizerCtrl() extends Component {

  import RxConfig._

  val dataIn = slave Stream testType()
  val dataOut = master Stream testType()

  val splitter = SplitterCtrl()
  val smooth = SmoothCtrl()
  val equal = EqualCtrl()

  val preambleFIFO = StreamFifo(testType(), 2)
  val dataFIFO = StreamFifo(testType(), 32)
  val postPreambleFIFO = StreamFifo(testType(), 2)

  dataIn >> splitter.dataIn
  splitter.preambleOut >> preambleFIFO.io.push
  splitter.dataOut >> dataFIFO.io.push

  preambleFIFO.io.pop >> smooth.preambleIn
  smooth.preambleOut >> postPreambleFIFO.io.push

  postPreambleFIFO.io.pop >> equal.preambleIn
  dataFIFO.io.pop >> equal.dataIn
  equal.dataOut >> dataOut

}

case class SimpleCtrl() extends Component{

}

/** when input data are not independent
 *
 */
case class ComplicatedCtrl(batchSize:Int, latency:Int) extends Component{

  import RxConfig._

  val dataIn = slave Stream testType()
  val dataOut = master Stream testType()

  val batchCounter = Counter(batchSize, inc = dataIn.fire)
  val batchBeforeProcess = Reg(Vec(testType(), batchSize))



}


case class RxCtrl(loopSize: Int, frameSize: Int, iteration: Int) extends Component {

  import RxConfig._

  val dataIn = slave Stream testType()
  val dataOut = master Stream testType()

  case class FTNLoop() extends Component { // the non-terminating loop

    val dataIn = slave Stream testType()

  }

}
