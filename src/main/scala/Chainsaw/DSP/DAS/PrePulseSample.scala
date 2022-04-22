package Chainsaw.DSP.DAS
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
case class PrePulseSample() extends Component {
  val io = new Io()

  io.dataOut.valid.clear()
  io.dataOut.payload := io.dataIn.payload

  val mergeWidth = (Para.pulseLength - Para.probeWidth) / 2

  val loadCounter = Counter(0, mergeWidth - 1)
  when(io.dataIn.valid) {
    when(~loadCounter.willOverflowIfInc)(loadCounter.increment())
  }

  val probeCounter   = Counter(0, Para.probeWidth - 1)
  val mergeCounter2 = Counter(0, mergeWidth * 2 - 1)

  val FSM = new StateMachine {
    val s1 = new State() with EntryPoint
    val s2 = new State()
    val s3 = new State()

    s1.whenIsActive {
      io.dataOut.valid.clear()
      when(loadCounter.willOverflowIfInc && io.dataIn.valid) {
        goto(s2)
      }
    }
    s2.whenIsActive {
      io.dataOut.valid := io.dataIn.valid
      countAndGo(probeCounter, s3)
    }
    s3.whenIsActive {
      io.dataOut.valid.clear()
      countAndGo(mergeCounter2, s2)
    }

    def countAndGo(c: Counter, s: State)={
      when(c.willOverflowIfInc && io.dataIn.valid){
        c.clear()
        goto(s)
      }elsewhen(io.dataIn.valid)(c.increment())
    }
  }
}
