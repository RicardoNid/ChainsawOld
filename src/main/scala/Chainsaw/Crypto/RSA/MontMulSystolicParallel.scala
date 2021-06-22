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
    val start = in Bool()
    val mode = in Bits (lMs.size bits) // one-hot, provided by the external control logic
    // data
    val xiIns = in Vec(UInt(1 bits), parallelFactor)
    val YWordIns = in Vec(UInt(w bits), parallelFactor)
    val MWordIns = in Vec(UInt(w bits), parallelFactor)

    val dataOuts = out Vec(UInt(w bits), parallelFactor)
    val valids = out Vec(Bool, parallelFactor)
    val idle = out Bool()
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
  val roundCounter = MultiCountCounter(rounds.map(BigInt(_)), io.mode, inc = eCounter.willOverflow)

  // datapath, see the diagram
  // for w >= 4, parallel p has the property that parallelP(2 * lM) = 2 * parallelP(lM), makes the design regular
  // more specifically, p = e - 1 for each size
  val PEs = (0 until p).map(_ => new MontMulPE(w))
  //  val groupNum = parallelFactor

  val groupStarters = PEs.indices.filter(_ % groupSize == 0).map(PEs(_))
  // TODO: caution: ender is different from the output
  val groupEnders = PEs.indices.filter(_ % groupSize == groupSize - 1).map(PEs(_))
  val buffers = groupEnders.map(pe => RegNext(pe.io.flowOut)) // queue with depth = 1
  buffers.foreach { buffer =>
    buffer.init(MontMulPEFlow(w).getZero)
    buffer.control.valid.allowOverride
    buffer.control.valid := False // TODO: valid would stop here, in fact, that register should be saved
  }
  // connecting PEs with each other
  val datapath = new Area {
    // the "default connection", all groups are connected one after another, inputPEs works as successors
    PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.flowIn := prev.io.flowOut }
    PEs.head.io.flowIn := MontMulPEFlow(w).getZero
    PEs.foreach(_.io.xi := U(0))

    val inputNow, setXiNow, setValidNow = Bool()
    Seq(inputNow, setXiNow, setValidNow).foreach(_.clear())

    val availables = Vec(Bool(), parallelFactor)
    availables := availables.getZero
    availables.allowOverride // some may be always available

    switch(True) {
      lMs.zipWithIndex.foreach { case (lM, i) =>
        val groupPerInstance = lM / lMs.min
        is(io.mode(i)) {
          println(s"mode = $i, an instance contains $groupPerInstance group and ${groupPerInstance * groupSize} PEs")
          groupStarters.zipWithIndex
            .filter { case (e, i) => i % groupPerInstance == 0 } // take the instance starters
            .take(parallelFactor / groupPerInstance) // drop the instance without enough length
            .foreach { case (starter, i) =>
              val wrapAroundBuffer = buffers(i + groupPerInstance - 1)
              starter.io.flowIn.data := Mux(inputNow, dataIns(i), wrapAroundBuffer.data)
              starter.io.flowIn.control.SetXi := Mux(setXiNow, setXiNow, wrapAroundBuffer.control.SetXi)
              starter.io.flowIn.control.valid := Mux(setValidNow, setValidNow, wrapAroundBuffer.control.valid)
              (i * groupSize until (i + groupPerInstance) * groupSize).foreach { id =>
                PEs(id).io.xi := io.xiIns(i) // FIXME: xi fanout is huge for 4096
              }
              availables(i) := True
            }
        }
        default {
          availables.foreach(_ := False)
        }
      }
    }
  }

  // FIXME: this is for RSA only, as 512,1024...are special
  println(s"outputProviders are ${outputProviders.mkString(" ")}")
  val outputPEs = outputProviders.map(PEs(_))
  io.dataOuts.zip(outputPEs).foreach { case (dataOut, pe) => dataOut := pe.io.flowOut.data.SWord }
  io.valids := Vec(datapath.availables.zip(outputPEs).map { case (avail, pe) => avail && pe.io.flowOut.control.valid })


  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val RUN = State()

    IDLE.whenIsActive {
      when(io.start)(goto(RUN))
    }

    RUN.whenIsActive { // control the dataflow
      eCounter.increment()
      when(roundCounter.value === U(0)) {
        // PE data input
        datapath.inputNow := True
        when(eCounter.value === U(0))(datapath.setXiNow := True)
      }
      when(roundCounter.willOverflowIfInc) { // the "will overflow if inc"
        datapath.setValidNow := True
      }
      when(roundCounter.willOverflow) {
        buffers.foreach(_.control.SetXi := False) // clean up the setXi from last task
        // this will lead the first bits of the most significant bit of S to be different from the original
        // just ignore that, as it is don't care anyway
        when(io.start)(goto(RUN))
          .otherwise(goto(IDLE))
      }
    }
    io.idle := isActive(IDLE)
  }

  val feedMYNow = out(roundCounter.value === U(0))
  // drop the msb, only valid for RSA, as word number without padding would be a power of 2
  val MYWordIndex = out(eCounter.value(eCounter.value.getBitsWidth - 2 downto 0))
  val feedXNow = out(eCounter.value < MuxOH(io.mode, parallelPs.map(U(_))))
  val padXNow = out(roundCounter.willOverflowIfInc)
  val lastCycle = out(roundCounter.willOverflow)
}

object MontMulSystolicParallel {
  def main(args: Array[String]): Unit = {
    GenRTL(new MontMulSystolicParallel(MontConfig(Seq(512, 1024, 2048, 3072, 4096), 32, 17, parallel = true)))
    //    VivadoSynth(new MontMulSystolicParallel(MontConfig(Seq(512, 1024, 2048, 3072, 4096), 32, 17, parallel = true)))
  }
}
