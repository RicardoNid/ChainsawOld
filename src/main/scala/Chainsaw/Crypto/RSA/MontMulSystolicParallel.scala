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
    val mode = in Bits (lMs.size bits) // one-hot
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

  val modeReg = Reg(HardType(io.mode))
  when(io.start)(modeReg := io.mode)

  // counters, set them with the maximum value and run them in different modes
  val eCounter = Counter(es.max)
  val eCounterWillOverflows = es.map(e => eCounter.value === U(e - 1) && eCounter.willIncrement)
  val currentECounterOverflow = MuxOH(modeReg, eCounterWillOverflows)
  when(currentECounterOverflow)(eCounter.clear())
  val roundCounter = Counter(rounds.max, inc = currentECounterOverflow)
  val roundCounterWillOverflows = rounds.map(round => roundCounter.value === U(round - 1) && roundCounter.willIncrement)
  val currentRoundCounterOverflow = MuxOH(modeReg, roundCounterWillOverflows)
  when(currentRoundCounterOverflow)(roundCounter.clear())

  // datapath, see the diagram
  // for w >= 4, parallel p has the property that parallelP(2 * lM) = 2 * parallelP(lM), makes the design regular
  // more specifically, p = e - 1 for each size
  val PEs = (0 until p).map(_ => new MontMulPE(w))
  //  val groupNum = parallelFactor

  val groupStarters = PEs.indices.filter(_ % groupSize == 0).map(PEs(_))
  val groupEnders = PEs.indices.filter(_ % groupSize == groupSize - 1).map(PEs(_))
  val buffers = groupEnders.map(pe => RegNext(pe.io.flowOut)) // queue with depth = 1
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
        is(modeReg(i)) {
          println(s"mode = $i, an instance contains $groupPerInstance group and ${groupPerInstance * groupSize} PEs")
          groupStarters.zipWithIndex
            .filter { case (e, i) => i % groupPerInstance == 0 } // take the instance starters
            .take(parallelFactor / groupPerInstance) // drop the instance without enough length
            .foreach { case (starter, i) =>
              println(s"for starter $i")
              starter.io.flowIn.data := Mux(inputNow, dataIns(i), buffers(i + groupPerInstance - 1).data)
              starter.io.flowIn.control.SetXi := setXiNow
              starter.io.flowIn.control.valid := setValidNow
              (i * groupSize until (i + groupPerInstance) * groupSize).foreach { id =>
                PEs(id).io.xi := io.xiIns(i)
              }
              availables(i + groupPerInstance - 1) := True
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
      when(roundCounter.value === MuxOH(modeReg, rounds.map(round => U(round - 1)))) { // the "will overflow if inc"
        datapath.setValidNow := True
      }
      when(currentRoundCounterOverflow) {
        when(io.start)(goto(RUN))
          .otherwise(goto(IDLE))
      }
    }
    io.idle := isActive(IDLE)
  }
}

object MontMulSystolicParallel {
  def main(args: Array[String]): Unit = {
    GenRTL(new MontMulSystolicParallel(MontConfig(Seq(512, 1024, 2048), 32, 17, parallel = true)))
    //    VivadoSynth(new MontMulPE(128))
  }
}
