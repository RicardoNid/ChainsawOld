package Chainsaw.crypto.ReedSolomon

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import GaloisFieldHardWare._

/** RS code (n, k)
  * @param n
  *   length of the code word
  * @param k
  *   number of message symbols
  * @param m
  *   bit width of symbol
  */
case class Encoding(n: Int, k: Int) extends Component {
  val m = n - k
  val io = new Bundle {
    val message = slave Stream (UInt(m bits))
    val output  = master Flow (UInt(m bits))
  }

  io.message.ready.set()

  val regs = Vec(Reg(UInt(m bits)) init (0), n - k)
  val coff = new RS(n, k).getGxCoff

  val loading = Bool()
  loading.clear()
  val loadData = Mux(io.message.valid && loading, io.message.payload.add(regs.head), U(0))
  val multiRes = coff.tail.map(loadData.constMulti(_))
  when(io.message.valid || (!loading)) {
    regs.init.zip(regs.tail).zip(multiRes.init).foreach { case ((r, rn), m) => r := rn.add(m) }
    regs.last := multiRes.last
  }

  val counter = Counter(0, n - 1)
  val FSM = new StateMachine {
    val encrypting = new State() with EntryPoint
    val done       = new State()

    encrypting.whenIsActive {
      loading.set()
      when(io.message.valid)(counter.increment())
      when(counter.value === k - 1 && io.message.valid) {
        goto(done)
      }
    }

    done.whenIsActive {
      loading.clear()
      io.message.ready.clear()
      counter.increment()
      when(counter.value === n - 1) {
        goto(encrypting)
      }
    }
  }
  io.output.payload := Mux(loading, io.message.payload, regs.head)
  io.output.valid   := Mux(loading, io.message.valid, True)
}
