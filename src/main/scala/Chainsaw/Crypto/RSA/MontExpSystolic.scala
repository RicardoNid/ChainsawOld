package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import cc.redberry.rings.bigint.BigInteger

case class MontExpSystolic(config: MontConfig,
                           rSquare: BigInt, M: BigInt, E: BigInt, ELength: Int,
                           Xs: Seq[BigInt]
                          ) extends Component {

  import config._

  require(isPow2(w))
  //  require(lMs.forall(lM => isPow2(lM / w))) // not valid for 3072

  val io = new Bundle {
    val start = in Bool()
    val mode = in Bits (lMs.size bits)
    //    val XWordIns = in Vec(UInt(w bits), parallelFactor)
    //    val rSquareWordIn = in UInt (w bits) // Y for the first MontMul
    //    val MWordIn = in UInt (w bits)
    //    val ExponentWordIn = in UInt (lMs.max bits)
    //    val ExponentLengthIn = in UInt (log2Up(lMs.max + 1) bits)
    val dataOut = out Vec(UInt(w bits), parallelFactor)
    val valids = out Vec(Bool, parallelFactor)
  }

  def report = {

  }

  val modeReg = Reg(HardType(io.mode))
  when(io.start)(modeReg := io.mode)

  // memories
  // TODO: use less than e?
  val Seq(rSquareWordRAM, mWordRAM, exponentWordRAM) = Seq(rSquare, M, E).map(bigint => Mem(toWords(bigint, w, lMs.max / w).map(U(_, w bits))))
  require(Xs.size == parallelFactor)

  val xWords = Xs.map(x => toWords(x, w, lMs.min / w))
  val xWordRAMs = xWords.map(XWord => Mem(XWord.map(U(_, w bits))))
  val partialProductWordRAMs = Seq.fill(parallelFactor)(Mem(UInt(w bits), lMs.min / w))

  val xBitCounter = Counter(w)
  val xWordCounter = Counter(lMs.min / w, inc = xBitCounter.willOverflow)
  println(s"RAMCounter counts = ${lMs.map(lM => BigInt(lM / lMs.min)).mkString(" ")}")
  val xRAMCounter = MultiCountCounter(lMs.map(lM => BigInt(lM / lMs.min)), modeReg, inc = xWordCounter.willOverflow) // how many RAMs should be involved

  val exponentLengthReg = RegInit(U(ELength))
  val mult = MontMulSystolicParallel(config)

  // pre-assignment
  mult.io.xiIns.foreach(_.clearAll())
  mult.io.start := io.start
  mult.io.mode := io.mode
  mult.io.YWordIns.foreach(_.clearAll())
  mult.io.MWordIns.foreach(_.clearAll())

  io.dataOut := mult.io.dataOuts
  io.valids := mult.io.valids

  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val WORK = new State()

    IDLE.whenIsActive {
      when(io.start)(goto(WORK))
    }

    WORK.whenIsActive {
      when(mult.feedXNow) { // describe how X is fed into MontMul
        xBitCounter.increment()
        switch(True) {
          lMs.zipWithIndex.foreach { case (lM, i) =>
            is(modeReg(i)) { // for each mode
              val ramStarters = xWordRAMs.indices.filter(_ % (lM / lMs.min) == 0)
              // FIXME: should a xWord be visited L2R or R2L?
              val xCandidates = Vec(xWordRAMs.map(ram => ram(xWordCounter.value)(xBitCounter.value))) // data from all xRAMs
              ramStarters.foreach { i =>
                mult.io.xiIns(i) := xCandidates(xRAMCounter.value + i).asUInt
              }
            }
          }
        }
      }
      when(mult.feedMYNow) {
        mult.io.MWordIns.foreach(_ := mWordRAM(mult.MYWordIndex))
        mult.io.YWordIns.foreach(_ := rSquareWordRAM(mult.MYWordIndex)) // only for RSA, as the true word number is a power of 2
      }
      when(mult.lastCycle)(mult.io.start := True)
    }
  }
}

object MontExpSystolic {
  def main(args: Array[String]): Unit = {
    val ref = new RSARef(512)
    val M = BigInt(ref.getModulus)
    val Xs = (0 until 8).map(_ => BigInt(ref.getPrivateValue) / DSPRand.nextInt(10000) - DSPRand.nextInt(10000))
    val E = BigInt(ref.getPrivateValue)
    val ELength = E.bitLength
    import cc.redberry.rings.scaladsl._
    val r = BigInt(1) << (M.bitLength + 2)
    val rSquare = BigInt(Zp(M)(r * r).toByteArray)
    //    GenRTL(new MontExpSystolic(MontConfig(parallel = true), rSquare, M, E, ELength, Xs))
    val result0 =  BigInt(Zp(M).multiply(r, Xs(0)).toByteArray)
    println("X0     : " + toWordsHex(Xs(0), 32, 16))
    println("M      : " + toWordsHex(M, 32, 16))
    println("rSquare: " + toWordsHex(rSquare, 32, 16))
    println("result:  " + toWordsHex(result0 << 1, 32, 16))
    println("result:  " + toWordsHex(if(result0  >= M) (result0 - M << 1)else result0 << 1, 32, 16))

    SimConfig.withWave.compile(new MontExpSystolic(MontConfig(parallel = true), rSquare, M, E, ELength, Xs)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
      io.start #= true
      io.mode #= BigInt(1) << 0
      sleep(5000)
    }
  }
}

