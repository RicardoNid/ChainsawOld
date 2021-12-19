package Chainsaw.DSP

import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class S2P[T <: Data](s: Int, p: Int, dataType: HardType[T])
  extends Component with DSPTestable[Vec[T], Vec[T]] {
  require(p % s == 0)
  override val dataIn = slave Stream Vec(dataType(), s)
  override val dataOut = master Stream Vec(dataType(), p)
  override val latency = 0

  val counter = Counter(p / s)
  when(counter =/= counter.getZero || dataIn.fire)(counter.increment())

  val buffers: Seq[Vec[T]] = Seq.fill(p / s - 1)(Reg(cloneOf(dataIn.payload)))

  dataIn.ready := True
  dataOut.valid := counter.value === U(p / s - 1)

  switch(counter.value) {
    (0 until p / s - 1).foreach(i => is(U(i)){
      buffers(i) := dataIn.payload
      dataOut.payload.assignDontCare()
    })
    is(U(p / s - 1)) (dataOut.payload := Vec(buffers.map(_.toSeq).reduce(_ ++ _) ++ dataIn.payload))
    if (!isPow2(p / s)) default(dataOut.payload.assignDontCare())
  }
}
