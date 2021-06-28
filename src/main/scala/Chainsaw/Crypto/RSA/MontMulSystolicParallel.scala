package Chainsaw.Crypto.RSA

import spinal.core._

case class MontMulSystolicParallel(config: MontConfig) extends Component {

  import config._

  val io = new Bundle {
    // control
    val mode = in Bits (lMs.size bits) // one-hot, provided by the external control logic
    val xiIns = in Vec(UInt(1 bits), parallelFactor)
    val YWordIns = in Vec(UInt(w bits), parallelFactor)
    val MWordIns = in Vec(UInt(w bits), parallelFactor)
    val firstRound, firstWord, lastCycle = in Bool()

    val dataOuts = out Vec(UInt(w bits), parallelFactor)
  }

  val dataIns = Vec(MontMulPEDataFlow(w), parallelFactor)
  dataIns.zipWithIndex foreach { case (dataIn, i) =>
    dataIn.SWord := U(0)
    dataIn.MWord := io.MWordIns(i)
    dataIn.YWord := io.YWordIns(i)
  }


  val PEs = (0 until p).map(_ => new MontMulPE(w))

  val groupStarters = PEs.indices.filter(_ % pPerGroup == 0).map(PEs(_))
  val groupEnders = PEs.indices.filter(_ % pPerGroup == pPerGroup - 1).map(PEs(_))
  val buffers = groupEnders.map(pe => RegNext(pe.io.flowOut)) // queue with depth = 1
  buffers.foreach(buffer => buffer.init(MontMulPEFlow(w).getZero)) // clear when not connected

  // connecting PEs with each other
  PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.flowIn := prev.io.flowOut }
  PEs.head.io.flowIn := MontMulPEFlow(w).getZero // pre-assign
  PEs.foreach(_.io.xi := U(0)) // pre-assign

  /**
   * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 "MontMult-Connections" in this page]]
   */
  val inputNow, setXiNow = Bool()
  Seq(inputNow, setXiNow).foreach(_.clear())

  when(io.firstRound) {
    inputNow := True
    when(io.firstWord)(setXiNow := True)
  }
  // FIXME: this will lead the first bits of the most significant bit of S to be different from the original just ignore that, as it is don't care anyway
  when(io.lastCycle)(buffers.foreach(_.control.SetXi := False)) // clean up the setXi from last task

  // BLOCK Connections
  switch(True) {
    lMs.indices.foreach { i =>
      val groupCount = groupPerInstance(i)
      val starterIds = startersAtModes(i)
      println(s"mode $i, starterIds = ${starterIds.mkString(" ")}")
      is(io.mode(i)) {
        println(s"mode = $i, an instance contains $groupCount group and ${groupCount * pPerGroup} PEs")
        starterIds.foreach { j =>
          val starter = PEs(j * pPerGroup)
          val wrapAroundBuffer = buffers(j + groupCount - 1)
          starter.io.flowIn.data := Mux(inputNow, dataIns(j), wrapAroundBuffer.data)
          starter.io.flowIn.control.SetXi := Mux(setXiNow, setXiNow, wrapAroundBuffer.control.SetXi)
          // TODO: try max_fanout/reg duplication as xi fanout is huge for 4096
          (j * pPerGroup until (j + groupCount) * pPerGroup).foreach(id => PEs(id).io.xi := io.xiIns(j))
        }
      }
    }
  }

  println(s"outputProviders are ${outputProviders.mkString(" ")}")
  val outputPEs = outputProviders.map(PEs(_))
  io.dataOuts.zip(outputPEs).foreach { case (dataOut, pe) => dataOut := pe.io.flowOut.data.SWord }
}