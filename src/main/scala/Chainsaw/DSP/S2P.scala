package Chainsaw.DSP

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

@outputRegistered
case class S2P[T <: Data](s: Int, p: Int, dataType: HardType[T])
  extends Component with DSPTestable[Vec[T], Vec[T]] {
  require(p % s == 0)
  override val dataIn = slave Stream Vec(dataType(), s)
  override val dataOut = master Stream Vec(dataType(), p)
  override val latency = p / s

  val counter = Counter(p / s)
  when(counter =/= counter.getZero || dataIn.fire)(counter.increment())

  val buffers: Seq[Vec[T]] = Seq.fill(p / s - 1)(Reg(cloneOf(dataIn.payload)))

  switch(counter.value) {
    (0 until p / s - 1).foreach(i => is(U(i)) {
      buffers(i) := dataIn.payload
    })
  }

  dataIn.ready := True
  val innerDataOut = cloneOf(dataOut)
  innerDataOut.valid := counter.value === U(p / s - 1)
  innerDataOut.payload := Vec(buffers.map(_.toSeq).reduce(_ ++ _) ++ dataIn.payload)

  dataOut.valid := RegNext(innerDataOut.valid, init = False)
  dataOut.payload := RegNext(innerDataOut.payload)
}
