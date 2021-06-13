//package Chainsaw.Crypto.RSA
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.sim._
//import spinal.lib.fsm._
//import Chainsaw._
//import Chainsaw.Real
//
//import scala.math.ceil
//
//// io ports for systolic array
//case class PEPort(w: Int) extends Bundle {
//  val Y = UInt(w bits)
//  val M = UInt(w bits)
//  // SE/SO ## SC ## S0 combines the S word
//  val SCommon = UInt(w - 2 bits) // bits w-2 downto 1
//  val S0 = Bool() // bit 0(input: next word, output: current word)
//}
//
//class PE(w: Int) extends Component {
//  val input = in(PEPort(w))
//  val setXi = in Bool()
//  val xiInput = in Bool()
//  val output = out(PEPort(w))
//
//  val qiStored = Reg(Bool())
//  val qiInuse = Bool()
//  val xiStored = Reg(Bool())
//  val xiInUse = Bool()
//
//  when(setXi) {
//    xiInUse := xiInput
//    xiStored := xiInput
//    qiInuse := (xiInUse & input.Y(0)) ^ input.SCommon(0)
//    qiStored := (xiInUse & input.Y(0)) ^ input.SCommon(0)
//  }.otherwise {
//    xiInUse := xiStored
//    qiInuse := qiStored
//  }
//  // these are somehow like accumulators, init 0 is needed
//  val CO, CE = RegInit(U(0, 2 bits))
//  val SO, SE = RegInit(False)
//  val SCommon = Reg(U(0, w - 1 bits)) // w-2 downto 0
//  // logic
//  // TODO: considering using and gates
//  val xiY = Mux(xiInUse, input.Y, U(0)) // w bits
//  val qiM = Mux(qiInuse, input.M, U(0)) // w bits
//  SCommon := // w+1 bits
//    (Mux(input.S0, CO, CE) +^ xiY(w - 2 downto 0)) + // 2 bits + w-1 bits -> w bits
//      (qiM(w - 2 downto 0) +^ input.SCommon) // w-1 bits + w-1 bits -> w bits
//  val HighO = (SCommon(w downto w - 1) +^ xiY.msb.asUInt) + (qiM.msb.asUInt +^ U(0, 1 bits)) // 3 bits
//  val HignE = (SCommon(w downto w - 1) +^ xiY.msb.asUInt) + (qiM.msb.asUInt +^ U(1, 1 bits)) // 3 bits
//  CO := HighO(2 downto 1)
//  CE := HignE(2 downto 1)
//  SO := HighO.lsb
//  SO := HighO.lsb
//
//  // output
//  output.SCommon := SCommon
//  output.S0 := Mux(input.S0, SO, SE)
//  output.M := RegNext(input.M)
//  output.Y := RegNext(input.Y)
//}
//
//// TODO: improve later
////case class MontMulInput(w: Int) extends Bundle {
////  val X = Bool()
////  val Y = UInt(w bits)
////  val M = UInt(w bits)
////}
//
//class MontMul(n: Int, w: Int, p: Int) extends Component {
//  //  val input = in(MontMulInput(w))
//  val output = out UInt (w bits)
//  val valid = out Bool()
//  val busy = RegInit(False)
//
//  val e = ceil((n + 1).toDouble / w).toInt // number of words
//
//  val validNs = Seq(512, 1024, 2048, 3072, 4096)
//  //  require(validNs.contains(n), s"n must be a legal RSA length, that is, ${validNs.mkString(" ")}")
//
//  // 2. p < n, p >= e
//  // 3. p < e, p >= 1
//
//  // datapath
//
//  // TODO: for test only
//  // bigtest
//  //  val ref = new RSARef(n)
//  //  val XForTest = BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000)
//  //  val YForTest = BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000)
//  //  val MForTest = BigInt(ref.getModulus)
//  //  val ROMs = Seq(XForTest, YForTest, MForTest).map(number => Mem(
//  //    number.toString(2).grouped(w).toSeq.map(BigInt(_, 2)).map(U(_, w bits))))
//  // small test, n = 8
//  val X = 159
//  val Y = 148
//  val M = 177
//  val ROMs = Seq(Y, M).map(number =>
//    Mem(BigInt(number).toString(2).padToLeft(8, '0')
//      .grouped(w).toSeq.map(BigInt(_, 2)).map(U(_, w bits))))
//  val YROM = ROMs(0)
//  val MROM = ROMs(1)
//  val XROM = Mem(Seq(
//    True, False, False, True,
//    True, True, True, True))
//
//  val systolicArray = new Area {
//    // 1. p = n
//    val PEs = (0 until n).map(_ => new PE(w))
//    // connecting PEs
//    PEs.init.zip(PEs.tail).foreach { case (pe0, pe1) => pe1.input := pe0.output }
//  }
//
//  val runningCounter = Counter(e)
//  val runningMask = RegInit(B("0" * (n - 1) + "1"))
//  val currentXi = XROM(runningCounter)
//
//  val dataPath = new Area {
//    when(runningCounter <= e) {
//      systolicArray.PEs(0).input.M := MROM(runningCounter.value)
//      systolicArray.PEs(0).input.Y := YROM(runningCounter.value)
//      systolicArray.PEs(0).input.S0 := False
//      systolicArray.PEs(0).input.SCommon := U(0)
//
//      systolicArray.PEs.zipWithIndex.foreach { case (pe, i) =>
//        pe.setXi := runningMask(i)
//        pe.xiInput := currentXi
//      }
//    }
//  }
//
//  val fsm = new StateMachine {
//    val IDLE = StateEntryPoint()
//    val RUN = new StateDelayFixed(e)
//
//    IDLE.whenIsActive {
//      goto(RUN)
//    }
//    RUN.whenIsActive {
//      runningCounter.increment()
//      when(runningCounter.willOverflow)(goto(RUN))
//    }
//  }
//
//  valid := Delay(busy, n + e - 1)
//}
//
//object MontMul {
//  def main(args: Array[String]): Unit = {
//    GenRTL(new MontMul(8, 4, 8))
//    SimConfig.withWave.compile(new MontExp(8, 4, 8)).doSim { dut =>
//      import dut._
//      clockDomain.forkStimulus(2)
//      clockDomain.waitSampling()
//      (0 until 100).foreach { _ =>
//        clockDomain.waitSampling()
//      }
//    }
//  }
//}
