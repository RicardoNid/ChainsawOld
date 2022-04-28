package Chainsaw.crypto.ReedSolomon
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import GaloisFieldHardWare._

case class ErrValue() extends Component {
  val io = new Bundle {
    val omegaRLambda = slave Stream (Vec(UInt(4 bits), 3))
    val errValue     = master Stream (UInt(4 bits))
  }

  io.omegaRLambda.ready.set()
  io.errValue.valid.clear()

  val calculating = Bool()
  calculating.clear()
  val accs = io.omegaRLambda.payload.init.zipWithIndex.map { case (p, i) =>
    acc(data = p, i = i, m = 4, calculating = calculating, valid = io.omegaRLambda.valid)
  }

  val count = Counter(0, 14)
  when(calculating)(count.increment())

  val sum = accs.map(_.accReg).reduce((l, r) => l.add(r)).keep()
  val res = sum.multi(io.omegaRLambda.payload.last.inv)

  val FSM = new StateMachine {
    val load = new State() with EntryPoint
    val calc = new State()

    load.whenIsActive {
      io.omegaRLambda.ready.set()
      when(io.omegaRLambda.valid && io.errValue.ready) {
        count.increment()
        goto(calc)
      }
    }

    calc.whenIsActive {
      calculating.set()
      io.omegaRLambda.ready.clear()
      when(count.willOverflow)(goto(load))
    }
  }

  io.errValue.payload := res
}
