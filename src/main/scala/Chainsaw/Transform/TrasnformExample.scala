package Chainsaw.Transform

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

case class TransformIn(width: Int) extends Bundle {
  val a, b = UInt(width bits)
}

case class TrasnformCoreExample(width: Int, accPeriod: Int) extends TransformCore[TransformIn, UInt] {

  override val dataIn = in(TransformIn(width))
  override val dataOut = out(UInt(width bits))
  override def timingInfo = TimingInfo(accPeriod, accPeriod, 1)

  val counter = Counter(accPeriod)
  when(start || counter =/= 0)(counter.increment())

  val partialSum = Reg(UInt(width bits))
  val sum = dataIn.a + dataIn.b
  when(counter === 0)(partialSum := sum)
    .otherwise(partialSum := partialSum + sum)

  ready := (counter.valueNext === 0)

  dataOut := partialSum
}

case class TrasnformWrapperExample(usingStream: Boolean, core: TransformCore[TransformIn, UInt], extraTimingInfo: TimingInfo) extends Component {

  val usingLast = core.timingInfo.inputInterval == -1
  val timingInfo = if (usingLast) extraTimingInfo else core.timingInfo

  import timingInfo._

  val typeIn = HardType(core.dataIn)
  val typeOut = HardType(core.dataOut)

  (usingStream && usingLast) generate {

    val dataIn = slave Stream Fragment(typeIn)
    val dataOut = master Stream Fragment(typeOut)

    val fifoIn = StreamFifo(Fragment(typeIn), 2 * inputInterval)
    dataIn >> fifoIn.io.push
    val fifoOut = StreamFifo(Fragment(typeOut), 2 * outputInterval)
    fifoOut.io.pop >> dataOut

    val counterIn = Counter(inputInterval)
    fifoIn.io.pop.ready := False

    // TODO: this lead to too long latency
    val good = fifoIn.io.occupancy >= inputInterval && fifoOut.io.availability >= outputInterval

    val start = (good && core.ready) || counterIn =/= 0
    when((good && core.ready) || counterIn =/= 0){
      counterIn.increment()
      fifoIn.io.pop.ready := True
    }
    dataOut.valid := fifoOut.io.occupancy >= outputInterval
  }
}

object TestCoreExample extends App {
  SimConfig.withWave.compile(TrasnformCoreExample(4, 3)).doSim { dut =>
    import dut.{clockDomain, dataIn, dataOut}

    case class Pair(a: Int, b: Int)

    val testCases = Seq((1, 2), (2, 1), (1, 1)).map(pair => Pair(pair._1, pair._2))

    def peek(pair: Pair) = {
      dataIn.a #= pair.a
      dataIn.b #= pair.b
    }

    def init() = {
      clockDomain.forkStimulus(2)
      dut.start #= false
      clockDomain.waitSampling()
    }

    def peekACase = {
      dut.timingInfo.inputInterval
    }

    dut.start #= true
    dut.dataIn.a #= 1
    dut.dataIn.b #= 2
    clockDomain.waitSampling()

    dut.start #= false
    dut.dataIn.a #= 2
    dut.dataIn.b #= 1
    clockDomain.waitSampling()

    dut.dataIn.a #= 1
    dut.dataIn.b #= 1
    clockDomain.waitSampling()

    clockDomain.waitSampling()

    clockDomain.waitSampling(3)
  }
}
