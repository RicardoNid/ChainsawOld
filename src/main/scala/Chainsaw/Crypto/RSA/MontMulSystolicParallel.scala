package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.math.{ceil, floor}

case class MontMulSystolicParallel(config: MontConfig) extends Component {

  import config._

  val io = new Bundle {
    // control
    val run = in Bool()
//    val start = in Bool()
    val mode = in Bits (lMs.size bits) // one-hot, provided by the external control logic
    // data
    val xiIns = in Vec(UInt(1 bits), parallelFactor)
//    xiIns.addAttribute("max_fanout = \"16\"")
    val YWordIns = in Vec(UInt(w bits), parallelFactor)
    val MWordIns = in Vec(UInt(w bits), parallelFactor)

    val dataOuts = out Vec(UInt(w bits), parallelFactor)
    val valids = out Vec(Bool, parallelFactor)
  }

  val dataIns = Vec(MontMulPEDataFlow(w), parallelFactor)
  dataIns.zipWithIndex foreach { case (dataIn, i) =>
    dataIn.SWord := U(0)
    dataIn.MWord := io.MWordIns(i)
    dataIn.YWord := io.YWordIns(i)
  }

  // counters, set them with the maximum value and run them in different modes
  // multi-mode counter, by reassign the "willOverflowIfInc"
  val eCounter = MultiCountCounter(es.map(BigInt(_)), io.mode)
  val roundCounter = Counter(rounds(0), inc = eCounter.willOverflow) // rounds are the same

  val PEs = (0 until p).map(_ => new MontMulPE(w))

  val groupStarters = PEs.indices.filter(_ % pPerGroup == 0).map(PEs(_))
  val groupEnders = PEs.indices.filter(_ % pPerGroup == pPerGroup - 1).map(PEs(_))
  val buffers = groupEnders.map(pe => RegNext(pe.io.flowOut)) // queue with depth = 1
  buffers.foreach { buffer =>
    buffer.init(MontMulPEFlow(w).getZero) // clear when not connected
//    buffer.control.valid.allowOverride
//    buffer.control.valid := False // TODO: valid would stop here, in fact, that register should be saved
  }
  // connecting PEs with each other
  val datapath = new Area {
    PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.flowIn := prev.io.flowOut }
    PEs.head.io.flowIn := MontMulPEFlow(w).getZero // pre-assign
    PEs.foreach(_.io.xi := U(0)) // pre-assign

    val inputNow, setXiNow, setValidNow = Bool()
    Seq(inputNow, setXiNow, setValidNow).foreach(_.clear())

    val availables = Vec(Bool(), parallelFactor)
    availables := availables.getZero

    /**
     * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 "MontMult-Connections" in this page]]
     */
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
//            starter.io.flowIn.control.valid := Mux(setValidNow, setValidNow, wrapAroundBuffer.control.valid)
            // TODO: try max_fanout/reg duplication as xi fanout is huge for 4096
            (j * pPerGroup until (j + groupCount) * pPerGroup).foreach(id => PEs(id).io.xi := io.xiIns(j))
            availables(j) := True
          }
        }
      }
    }
  }

  // BLOCK STATE MACHINE
  val fsm = new Area {
    
    // for readability, we pre-define some "timing" at the beginning
    val feedMYNow = out(roundCounter.value === U(0) && io.run)
    val MYWordIndex = out(eCounter.value(eCounter.value.getBitsWidth - 2 downto 0))
    val feedXNow = out(eCounter.value < MuxOH(io.mode, pPerInstance.map(U(_))) && io.run)
    val firstRount = (roundCounter.value === U(0) && io.run)
    val lastRound = out(roundCounter.willOverflowIfInc && io.run)
    val lastWord = out(eCounter.willOverflowIfInc && io.run)
    val lastCycle = out(roundCounter.willOverflow && io.run)


    println(s"outputProviders are ${outputProviders.mkString(" ")}")
    val outputPEs = outputProviders.map(PEs(_))
    io.dataOuts.zip(outputPEs).foreach { case (dataOut, pe) => dataOut := pe.io.flowOut.data.SWord }
    //  io.valids := Vec(datapath.availables.zip(outputPEs).map { case (avail, pe) => avail && pe.io.flowOut.control.valid })
    val resultValid = Delay(roundCounter.willOverflowIfInc, 2, init = False)
    io.valids := Vec(datapath.availables.map(_ && resultValid))

    when(io.run)(eCounter.increment())
    when(firstRount) {
      datapath.inputNow := True
      when(eCounter.value === U(0))(datapath.setXiNow := True)
    }
    when(lastRound) { // the "will overflow if inc"
      datapath.setValidNow := True
    }
    when(lastCycle) {
      buffers.foreach(_.control.SetXi := False) // clean up the setXi from last task
      // FIXME: this will lead the first bits of the most significant bit of S to be different from the original just ignore that, as it is don't care anyway
    }
  }
}