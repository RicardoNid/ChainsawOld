//package Chainsaw.Crypto.RSA
//
//import Chainsaw._
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//
//import scala.math.{ceil, floor}
//
//case class MontMulSystolicParallel(config: MontConfig) extends Component {
//
//  import config._
//
//  val io = new Bundle {
//    // control
//    val start = in Bool()
//    val mode = in Bits (lMs.size bits) // one-hot
//    // data
//    val xiIn = in Vec(UInt(1 bits), parallelFactor)
//    val YWordIn = in Vec(UInt(w bits), parallelFactor)
//    val MWordIn = in Vec(UInt(w bits), parallelFactor)
//
//    val dataOut = out Vec(UInt(w bits), parallelFactor)
//    val valid = out Bool() // share the same valid, as async mode is not supported yet
//    val idle = out Bool()
//  }
//
//  val modeReg = Reg(HardType(io.mode))
//  when(io.start)(modeReg := io.mode)
//
//  // TODO: clean up this part
//  // for w >= 4, parallel p has the property that parallelP(2 * lM) = 2 * parallelP(lM), makes the design regular
//  val PEs = (0 until p).map(_ => new MontMulPE(w))
//  val basicSize = p / parallelFactor // PEs number for
//  val inputPEs = PEs.indices.filter(_ % basicSize == 0).map(PEs(_))
//  val outputPEs = PEs.indices.filter(_ % basicSize == basicSize - 1).map(PEs(_))
//  // connecting PEs with each other
//  val datapath = new Area {
//    PEs.init.zip(PEs.tail).foreach { case (prev, next) => next.io.dataIn := prev.io.dataOut } // the "default connection", all groups are connected
//    //    val swithces = Vec(Bool, parallelFactor) // switches that control the connection
//    val inputCodes = lMs.map { lM => // look at the diagram
//      val length = lM / lMs.min
//      val number = parallelFactor / length
//      B((("1" + "0" * (length - 1)) * number).padTo(parallelFactor, '0').reverse) // for example, for size (512,1024,2048), mode 1024 has input code "1010"
//    }
//
//    val outputCodes = lMs.map { lM => // look at the diagram
//      val length = lM / lMs.min
//      val number = parallelFactor / length
//      B((("0" + "1" * (length - 1)) * number).padTo(parallelFactor, '0').reverse) // for example, for size (512,1024,2048), mode 1024 output code "0101"
//    }
//
//    inputPEs.head.io.dataIn.YWord := io.YWordIn.head
//    inputPEs.head.io.dataIn.MWord := io.MWordIn.head
//    inputPEs.zipWithIndex.tail.foreach { case (pe, i) =>
//      pe.io.dataIn.YWord := Mux(MuxOH(modeReg, inputCodes)(i), io.YWordIn(i), outputPEs(i - 1).io.dataOut.YWord)
//    }
//
//    // datapaths
//
//    PEs.foreach { pe =>
//      pe.io.controlIn := ???
//    }
//    val outputPEs = outputProviders.map(PEs(_))
//    val outputCandidates = outputPEs.map(pe =>
//      ((pe.io.dataOut.SComp ## pe.io.dataOut.S0).asUInt, pe.io.dataOut.valid))
//    //  val outputCandidates = outputProviders.map(outputProvider =>
//    //    (PEs(outputProvider).io.dataOut.SComp ## PEs(outputProvider).io.dataOut.S0).asUInt)
//    io.dataOut := MuxOH(modeReg, outputCandidates.map(_._1))
//    io.valid := MuxOH(modeReg, outputCandidates.map(_._2))
//  }
//
//  // PE control inputs
//  PEs.zipWithIndex.foreach { case (pe, i) =>
//    pe.io.controlIn.xi := io.xiIn
//  }
//
//  // counters
//  val eCounter = Counter(es.max)
//  val eCounterWillOverflows = es.map(e => eCounter.value === U(e - 1) && eCounter.willIncrement)
//  val currentECounterOverflow = MuxOH(modeReg, eCounterWillOverflows)
//  when(currentECounterOverflow)(eCounter.clear())
//  val roundCounter = Counter(rounds.max, inc = MuxOH(modeReg, eCounterWillOverflows))
//
//  val fsm = new StateMachine {
//    val IDLE = StateEntryPoint()
//    val RUN = State()
//
//    IDLE.whenIsActive {
//      when(io.start)(goto(RUN))
//    }
//
//    RUN.whenIsActive { // control the dataflow
//      eCounter.increment()
//      when(roundCounter.value === U(0)) {
//        // PE data input
//        PEs.head.io.dataIn.S0 := U(0)
//        PEs.head.io.dataIn.SComp := U(0)
//        PEs.head.io.dataIn.MWord := io.MWordIn
//        PEs.head.io.dataIn.YWord := io.YWordIn
//        PEs.head.io.dataIn.SetXi := False
//        when(eCounter.value === U(0))(PEs.head.io.dataIn.SetXi := True)
//      }
//      when(roundCounter.value === MuxOH(modeReg, rounds.map(round => U(round - 1)))) {
//        PEs.head.io.dataIn.valid := True
//      }
//      when(roundCounter === MuxOH(modeReg, rounds.map(round => U(round - 1))) && currentECounterOverflow) {
//        when(io.start)(goto(RUN))
//          .otherwise(goto(IDLE))
//      }
//    }
//    RUN.onExit(roundCounter.clear())
//
//    io.idle := isActive(IDLE)
//  }
//}
//
//object MontMulSystolic {
//  def main(args: Array[String]): Unit = {
//    //    GenRTL(new MontMulPE(16))
//    VivadoSynth(new MontMulPE(128))
//  }
//}
