package Chainsaw.examples

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// common logic
abstract class Exercise() extends Component {

  val charIn: DataCarrier[UInt]
  val charOut: DataCarrier[UInt]

  val countIn = Counter(128)
  val countDropped = Counter(128)
  val countOut = Counter(128)

  out(countIn.value)
  out(countDropped.value)
  out(countIn.value)

  val asciiValid = charIn.payload >= 32 && charIn.payload <= 127

  // renaming
  charIn.payload.setName("in_data")
  charIn.valid.setName("in_valid")
  charOut.payload.setName("out_data")
  charOut.valid.setName("out_valid")
  countIn.value.setName("count_char_in")
  countDropped.value.setName("count_char_dropped")
  countOut.value.setName("count_char_out")

}

case class Exercise0() extends Exercise {

  override val charIn = slave Flow UInt(8 bits)
  override val charOut = master Flow UInt(8 bits)

  when(charIn.valid)(countIn.increment())
  when(~asciiValid)(countDropped.increment())
  when(asciiValid)(countOut.increment())

  charOut.payload := RegNext(charIn.payload)
  charOut.valid := RegNext(asciiValid)

}


case class Exercise1() extends Exercise {

  override val charIn = slave Flow UInt(8 bits)
  override val charOut = master Flow UInt(8 bits)

  val modeValid = Bool()
  val isUpper = charIn.payload >= 65 && charIn.payload <= 90
  val isLower = charIn.payload >= 97 && charIn.payload <= 122
  val toUpper = charIn.payload - 32
  val toLower = charIn.payload + 32

  when(charIn.valid)(countIn.increment())
  when(~modeValid)(countDropped.increment())
  when(modeValid)(countOut.increment())

  val transitionOnNextCycle = RegNext(charIn.payload === "#".head.toInt)
  transitionOnNextCycle.init(False)

  val fsm = new StateMachine {
    val OFF = StateEntryPoint()
    val PASS, UPPER, LOWER = new State()

    // transition logic
    Seq(OFF, PASS, UPPER, LOWER).foreach(
      _.whenIsActive {
        when(transitionOnNextCycle) {
          switch(charIn.payload) {
            is(U("O".head.toInt))(goto(OFF))
            is(U("P".head.toInt))(goto(PASS))
            is(U("U".head.toInt))(goto(UPPER))
            is(U("L".head.toInt))(goto(LOWER))
            default() // do nothing
          }
        }
      }
    )

    // output logic
    OFF.whenIsActive {
      modeValid := False
      charOut.payload := RegNext(charIn.payload)
    }
    PASS.whenIsActive {
      modeValid := asciiValid
      charOut.payload := RegNext(charIn.payload)
    }
    UPPER.whenIsActive {
      modeValid := asciiValid
      charOut.payload := RegNext(Mux(isLower, toUpper, charIn.payload))
    }
    LOWER.whenIsActive {
      modeValid := asciiValid
      charOut.payload := RegNext(Mux(isUpper, toLower, charIn.payload))
    }
  }

  charOut.valid := RegNext(modeValid)
}