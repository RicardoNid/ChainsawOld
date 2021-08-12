package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import spinal.core.Component.{current, pop}


/** To implement the Rx of FTN, replace Train and passengerType by the codec iteration and frameType
 *
 * @param trainSize the latency of one iteration
 * @param carSize   the length of a frame
 * @param iteration the number that a frame should be iteratively coded/decoded for
 */
case class TrainCtrl(trainSize: Int, carSize: Int, iteration: Int) extends Component {

  def passengerType() = UInt(14 bits) // use the MSB as a flag of the head of the car

  val input = slave Stream passengerType()
  val output = master Stream passengerType()

  case class Train() extends Component {
    val passengerIn = in(passengerType())
    val passengerOut = out(passengerType())
    val sideIn = in(passengerType())

    val others = Delay(passengerIn, trainSize - 4, init = passengerType().getZero)
    val IFFTFFT = Delay(others, 2, init = passengerType().getZero)
    val sideway = Delay(others, 2, init = passengerType().getZero)

    val sub0_0 = IFFTFFT
    //    val sub0_1 = sideway
    val sub0_1 = passengerOut.getZero
    val sub0_ret = RegNext(sub0_0 - sub0_1)

    val sub1_0 = sub0_ret
    val sub1_1 = sideIn
    val sub1_ret = RegNext(sub1_0 - sub1_1)

    passengerOut := sub1_ret
  }

  val FDE = Chainsaw.FIFO(passengerType(), 16)
  val train = Train()

  val carCount = trainSize / carSize + 1
  val carIterCounters = Seq.fill(carCount)(Counter(iteration + 2))
  val carIterCounterTriggers = Vec(carIterCounters.map(_.willIncrement))
  val carIters = Vec(carIterCounters.map(_.value))

  val odometer = Counter(trainSize)
  val carmeter = Counter(carSize)
  val carArrive = carmeter.value === 0
  val carWillArrive = carmeter.valueNext === 0
  carWillArrive.simPublic()

  val carCounter = Counter(carCount)
  val currentCar = carCounter.value
  val nextCar = carCounter.valueNext

  val currentCarIter = carIters(currentCar)
  val nextCarIter = carIters(nextCar)

  val badCar = if (trainSize % carSize == 0) False else currentCar === carCount - 1

  val trainStation = new StateMachine {
    val EMPTY = StateEntryPoint()
    val RUNNING = State()

    EMPTY.whenIsActive(when(input.fire) {
      odometer.increment()
      carmeter.increment()
      goto(RUNNING)
    })

    RUNNING.whenIsActive {

      odometer.increment()

      when(odometer.willOverflow)(carmeter.clear())
        .otherwise(carmeter.increment())

      when(carIterCounters.map(_.value === 0).andR) {
        goto(EMPTY)
        odometer.clear()
        carmeter.clear()
      }
    }

    val countingService = new Area { //
      when(carWillArrive && isActive(RUNNING))(carCounter.increment())
      when(odometer.willOverflow)(carCounter.increment())
      when(carWillArrive && nextCarIter =/= 0)(carIterCounterTriggers(nextCar) := True)
    }

    val isGettingOn = RegNext(input.fire, init = False)
    val getOnService = new Area {
      when(carArrive && (currentCarIter === 0 || currentCarIter === iteration + 1) && !badCar) {
        input.ready := True
        when(input.fire)(carIterCounterTriggers(currentCar) := True)
      }
        .elsewhen(!carArrive && isGettingOn)(input.ready := True)
        .otherwise(input.ready := False)
    }

    val getOffCounter = Counter(carSize)
    when(getOffCounter.value =/= 0)(getOffCounter.increment())

    val getOffService = new Area {
      when(carArrive && currentCarIter === iteration + 1){
        output.valid := True
        getOffCounter.increment()
        when(input.fire)(carIters(currentCar) := U(1, log2Up(carCount) bits)) // when getOn and getOff happen together, skip the stage of empty
          .otherwise(carIterCounterTriggers(currentCar) := True)
      }
        .elsewhen(getOffCounter.value =/= 0)(output.valid := True)
        .otherwise(output.valid := False)
    }

  }

  train.passengerIn := Mux(input.fire, input.payload, train.passengerOut)
  train.sideIn := passengerType().getZero
  output.payload := train.passengerOut

  // for testing
  val fire = out(input.fire)
  val busy = out(currentCarIter =/= 0 || input.fire)
  val validOutput = out(Mux(output.valid, train.passengerOut, passengerType().getZero))
}

object TrainCtrl extends App {

}
