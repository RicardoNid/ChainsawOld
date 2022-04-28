package Chainsaw.crypto.ReedSolomon

import spinal.core._
import spinal.lib._
import GaloisFieldHardWare._
import spinal.lib.fsm._

case class ErrorLocationCalc(m: Int) extends Component {
  val io = new Bundle {
    val lambda          = slave Stream (Vec(UInt(m bits), 3))
    val positionRLambda = master Flow (Vec(UInt(m bits), 2)) // 0 -> position, 1 -> rLambda
  }

  io.lambda.ready.set()

  val calculating = Bool()
  calculating.clear()

  val gfm  = GaloisField(m)
  val accs = io.lambda.payload.zipWithIndex.map { case (p, i) => acc(p, i, m, calculating, io.lambda.valid) }
  val sum  = accs.map(_.accReg).reduce(_.add(_))

  val isZero  = sum === 0
  val counter = Counter(0, (1 << m) - 2)
  when(!io.lambda.ready)(counter.increment())

  val FSM = new StateMachine {
    val load = new State() with EntryPoint
    val calc = new State()

    load.whenIsActive {
      io.lambda.ready.set()
      when(io.lambda.valid) {
        goto(calc)
        counter.increment()
      }

      calc.whenIsActive {
        calculating.set()
        io.lambda.ready.clear()
        when(counter.willOverflowIfInc) {
          goto(load)
          counter.clear()
        }
      }
    }
  }

  io.positionRLambda.valid      := isZero && calculating
  io.positionRLambda.payload(0) := counter
  io.positionRLambda.payload(1) := accs(1).accReg

}
