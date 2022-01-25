package Chainsaw.DSP

import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class P2S[T <: Data](p: Int, s: Int, dataType: HardType[T])
  extends Component with DSPTestable[Vec[T], Vec[T]] {
  require(p % s == 0)
  override val dataIn = slave Stream Vec(dataType(), p)
  override val dataOut = master Stream Vec(dataType(), s)
  override val latency = 1

  val counter = Counter(p / s)
  when(counter =/= counter.getZero || dataIn.fire)(counter.increment())

  val groupedDataIn: Seq[Vec[T]] = dataIn.payload.grouped(s).toSeq.map(Vec(_))
  val buffers: Seq[Vec[T]] = Seq.fill(p / s - 1)(Reg(cloneOf(dataOut.payload)))

  val innerDataOut = cloneOf(dataOut)
  switch(counter.value) {
    is(U(0)) {
      buffers.zip(groupedDataIn.tail).foreach { case (buf, in) => buf := in }
      innerDataOut.payload := groupedDataIn.head
    }
    (1 until p / s).foreach(i => is(U(i))(innerDataOut.payload := buffers(i - 1)))
    if (!isPow2(p / s)) default(innerDataOut.payload.assignDontCare())
  }

  dataIn.ready := counter === counter.getZero
  innerDataOut.valid := counter =/= counter.getZero || dataIn.fire

  dataOut.valid := RegNext(innerDataOut.valid, init = False)
  dataOut.payload := RegNext(innerDataOut.payload)
}
