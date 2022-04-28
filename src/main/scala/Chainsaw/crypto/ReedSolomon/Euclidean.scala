package Chainsaw.crypto.ReedSolomon

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import GaloisFieldHardWare._
import spinal.lib.fsm._

case class Euclidean() extends Component { // FIXME
  val io = new Bundle {
    val S                = slave Stream (Vec(UInt(4 bits), 4))
    val gammaLambdaOmega = master Stream (Vec(UInt(4 bits), 5))
  }

  io.S.ready.set()
  io.gammaLambdaOmega.valid.clear()

  val A, B   = Seq.fill(4)(Reg(UInt(4 bits)) init (0))
  val C      = Seq.fill(2)(Reg(UInt(4 bits)) init (0))
  val D      = Seq.fill(3)(Reg(UInt(4 bits)) init (0))
  val Da     = Seq.fill(3)(UInt(4 bits))
  val output = Seq.fill(5)(Reg(UInt(4 bits)) init (0))
  Da.foreach(_.clearAll())

  val B3DA3 = B(3).multi(A(3).inv)
  val Ap    = A.init.map(_.multi(B3DA3))
  val Ba    = Ap.zip(B.init).map { case (a, b) => a.add(b) }
  val Cp    = C.map(_.multi(B3DA3))

  val FSM = new StateMachine {
    val load   = new State() with EntryPoint
    val loadBD = new State()
    val loadAC = new State()
    val done   = new State()

    load.whenIsActive {
      io.S.ready.set()
      A.zip(io.S.payload).foreach { case (a, s) => a := s }
      C.head           := 1
      C.last           := 0
      B.last           := 1
      B.init.foreach(_ := 0)
      D.foreach(_      := 0)
      when(io.S.valid)(goto(loadBD))
    }

    loadBD.whenIsActive {
      io.S.ready.clear()
      B.tail.zip(Ba).foreach { case (b, ba) => b := ba }
      B.head  := 0
      Da.head := D.head
      Da.tail.zip(D.tail).zip(Cp).foreach { case ((a, d), p) => a := d.add(p) }
      Da.zip(D).foreach { case (a, d) => d := a }
      goto(loadAC)
    }

    loadAC.whenIsActive {
      io.S.ready.clear()
      B.zip(A).foreach { case (b, a) => b := a }
      D.init.zip(C).foreach { case (d, c) => d := c }
      A.tail.zip(Ba).foreach { case (a, ba) => a := ba }
      A.head  := 0
      D.last  := 0
      Da.last := D.last
      Da.init.zip(D.init).zip(Cp).foreach { case ((a, d), p) => a := d.add(p) }
      C.zip(Da).foreach { case (c, a) => c := a }
      goto(loadBD)
      when(Ba.head === 0 || Ba.last === 0) {
        output.slice(0, 3).zip(Da).foreach { case (o, da) => o := da }
        output.slice(3, 5).zip(Ba.tail).foreach { case (o, ba) => o := ba }
        goto(done)
      }

      done.whenIsActive {
        io.gammaLambdaOmega.valid.set()
        when(io.gammaLambdaOmega.ready)(goto(load))
      }
    }
  }

  io.gammaLambdaOmega.payload.zip(output).foreach { case (g, o) => g := o }
}
