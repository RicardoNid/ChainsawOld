package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class MontExpSystolic(config: MontConfig,
                           rSquare: BigInt, M: BigInt, E: BigInt, ELength: Int,
                           Xs: Seq[BigInt]
                          ) extends Component {

  import config._

  require(isPow2(w))
  require(Xs.size == parallelFactor)
  //  require(lMs.forall(lM => isPow2(lM / w))) // not valid for 3072

  // TODO: encapsulate the MontMul datapath for PRE/POST/MULT/SQUARE to reuse
  // TODO: test for the whole MontExp process
  // TODO: test for different modes

  val io = new Bundle {
    val start = in Bool()
    val mode = in Bits (lMs.size bits)
    //    val XWordIns = in Vec(UInt(w bits), parallelFactor)
    //    val rSquareWordIn = in UInt (w bits) // Y for the first MontMul
    //    val MWordIn = in UInt (w bits)
    //    val ExponentWordIn = in UInt (lMs.max bits)
    //    val ExponentLengthIn = in UInt (log2Up(lMs.max + 1) bits)
    val dataOuts = out Vec(UInt(w bits), parallelFactor)
    val valids = out Vec(Bool, parallelFactor)
  }

  def report = {

  }

  // operator
  // TODO: use less than e?
  val modeReg = Reg(HardType(io.mode))
  val montMult = MontMulSystolicParallel(config)
  // pre-assignment
  montMult.io.xiIns.foreach(_.clearAll())
  montMult.io.start := io.start
  montMult.io.YWordIns.foreach(_.clearAll())
  montMult.io.MWordIns.foreach(_.clearAll())
  montMult.io.YWordIns.foreach(_.allowOverride)

  // memories
  val Seq(rSquareWordRAM, mWordRAM, exponentWordRAM) = Seq(rSquare, M, E).map(bigint => Mem(toWords(bigint, w, lMs.max / w).map(U(_, w bits))))
  val xWords = Xs.map(x => toWords(x, w, wordPerGroup))
  val squareRAMs = xWords.map(XWord => Mem(XWord.map(U(_, w bits)))) // at the beginning, x^0 = x
  val productRAMs = Seq.fill(parallelFactor)(Mem(UInt(w bits), wordPerGroup))
  val squareLasts, productLasts = Seq.fill(parallelFactor)(RegInit(U(0, w bits)))

  // counters
  // counters for x,y,modulus,and exponent
  // these are starters, who are triggered when corresponding operations occur
  val xBitCounter, exponentBitCounter = Counter(w)
  val yWordCounter, outputWordCounter = Counter(wordPerGroup)
  val mWordCounter = MultiCountCounter(lMs.map(lM => BigInt(lM / w)), modeReg)
  // cascaded counters driven by starters
  val xWordCounter = Counter(wordPerGroup, inc = xBitCounter.willOverflow)
  println(s"RAMCounter counts = ${lMs.map(lM => BigInt(lM / lMs.min)).mkString(" ")}")
  // how many RAMs should be involved in an instance
  val xRAMCounter = MultiCountCounter(groupPerInstance.map(BigInt(_)), modeReg, inc = xWordCounter.willOverflow)
  val yRAMCounter = MultiCountCounter(groupPerInstance.map(BigInt(_)), modeReg, inc = yWordCounter.willOverflow)
  val outputRAMCounter = MultiCountCounter(groupPerInstance.map(BigInt(_)), modeReg, inc = outputWordCounter.willOverflow)
  val exponentWordCounter = MultiCountCounter(lMs.map(lM => BigInt(lM / w)), modeReg, inc = exponentBitCounter.willOverflow)
  val exponentLengthReg = RegInit(U(ELength))

  val exponentCurrentBit = exponentWordRAM(exponentWordCounter.value)(exponentBitCounter.value)

  io.dataOuts := montMult.io.dataOuts
  io.valids := montMult.io.valids

  when(io.start) {
    modeReg := io.mode
    montMult.io.mode := io.mode
  }.otherwise {
    montMult.io.mode := modeReg
  }

  def readRAMsBit(rams: Seq[Mem[UInt]], wordId: UInt, bitId: UInt) = Vec(rams.map(ram => ram(wordId)(bitId)))
  def readRAMsWord(rams: Seq[Mem[UInt]], wordId: UInt) = Vec(rams.map(ram => ram(wordId)))

  // control the write back behavior
  val writeProduct = RegInit(False)
  val writeSquare = RegInit(False)
  val outputBuffers = montMult.io.dataOuts.map(RegNext(_)) // delay t for shift
  val validNext = RegNext(montMult.io.valids(0)) // delay valid for one cycle
  validNext.init(False)
  val datasToWrite = Vec(UInt(w bits), parallelFactor)
  datasToWrite.foreach(_.clearAll())
  val outputRAMEnables = Vec(Bool, parallelFactor)
  outputRAMEnables.foreach(_.clear())
  val writeBackLastWord = RegNext(outputRAMCounter.willOverflow)
  writeBackLastWord.init(False)
  (0 until parallelFactor).foreach{i =>
    squareRAMs(i).write(
      address = outputWordCounter.value,
      data = datasToWrite(i),
      enable = outputRAMEnables(i) && writeSquare && validNext
    )
    productRAMs(i).write(
      address = outputWordCounter.value,
      data = datasToWrite(i),
      enable = outputRAMEnables(i) && writeProduct && validNext
    )
  }


  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    //    val WORK = new State()
    val PRE, MULT, SQUARE, POST = new State()

    IDLE.whenIsActive {
      when(io.start)(goto(PRE))
    }

    // FIXME: should a xWord be visited L2R or R2L?
    PRE.whenIsActive {
      when(montMult.fsm.lastCycle)(montMult.io.start := True)
    }

    switch(True) { // for different modes
      lMs.indices.foreach { i => // traverse each mode
        // characteristics of this mode
        val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(i) == 0) // instance index
        is(modeReg(i)) { // for each mode
          // feed X
          when(montMult.fsm.feedXNow) { // describe how X is fed into MontMul
            val xCandidates = Vec(Bool, parallelFactor) // prepared for different modes, for each instance
            xCandidates.foreach(_.clear())
            when(!montMult.fsm.lastRound) {
              xBitCounter.increment()
              when(isActive(PRE) || isActive(MULT) || isActive(SQUARE)) {
                xCandidates := readRAMsBit(squareRAMs, xWordCounter, xBitCounter)
              }.elsewhen(isActive(POST)) {
                xCandidates := readRAMsBit(productRAMs, xWordCounter, xBitCounter)
              }
            }.otherwise {
              val xCandidates = Vec(Bool, parallelFactor)
              when(isActive(PRE) || isActive(MULT) || isActive(SQUARE)) {
                xCandidates := Vec(squareLasts.map(word => word(xBitCounter.value)))
              }.elsewhen(isActive(POST)) {
                xCandidates := Vec(productLasts.map(word => word(xBitCounter.value)))
              }
            }
            starterIds.foreach(j => montMult.io.xiIns(j) := xCandidates(xRAMCounter.value + j).asUInt) // feed X
          }
          // feed Y and Modulus
          when(montMult.fsm.feedMYNow) {

            val yCandidates = Vec(UInt(w bits), parallelFactor) // prepare for different modes, for each instance
            yCandidates.foreach(_.clearAll()) // pre - assignment
            def push0() = yCandidates := Vec(Seq.fill(parallelFactor)(U(0, w bits)))
            def push1() = yCandidates := Vec(Seq.fill(parallelFactor)(U(1, w bits)))

            when(!montMult.fsm.lastWord) {
              yWordCounter.increment()
              mWordCounter.increment()
              montMult.io.MWordIns.foreach(_ := mWordRAM(mWordCounter)) // feed M
              // only for RSA, as the true word number is a power of 2
              when(isActive(PRE))(yCandidates := Vec(Seq.fill(parallelFactor)(rSquareWordRAM(yRAMCounter @@ yWordCounter)))) // feed Y
                .elsewhen(isActive(MULT))(yCandidates := readRAMsWord(productRAMs, yWordCounter.value))
                .elsewhen(isActive(SQUARE))(yCandidates := readRAMsWord(squareRAMs, yWordCounter.value))
                .elsewhen(isActive(POST)) { // feed 1
                  when(yWordCounter.value === U(0))(push1())
                    .otherwise(push0())
                }
            }.otherwise {
              starterIds.foreach(j => montMult.io.YWordIns(j) := U(0, w bits)) // feed Y
              when(isActive(PRE))(push0()) // msw = 0
                .elsewhen(isActive(MULT))(yCandidates := Vec(productLasts))
                .elsewhen(isActive(SQUARE))(yCandidates := Vec(squareLasts))
                .elsewhen(isActive(POST))(push0()) // msw = 0
            }
            starterIds.foreach(j => montMult.io.YWordIns(j) := yCandidates(yRAMCounter.value + j))
          }
          // fetch XYR^-1 and write back
          when(montMult.fsm.lastRound) {
            when(isActive(PRE) && exponentCurrentBit) {
              writeSquare.set()
              writeProduct.set()
            }.elsewhen(isActive(PRE) || isActive(SQUARE)) {
              writeSquare.set()
              writeProduct.clear()
            }.elsewhen(isActive(MULT)) {
              writeSquare.clear()
              writeProduct.set()
            }.otherwise {
              writeSquare.clear()
              writeProduct.clear()
            }
          }
          when(validNext) {
            when(!writeBackLastWord){
              outputWordCounter.increment()
              starterIds.foreach { j =>
                val data = (io.dataOuts(j).lsb ## outputBuffers(j)(w - 1 downto 1)).asUInt
                // design: selective write
                datasToWrite(j + outputRAMCounter.value) := data
                outputRAMEnables(j + outputRAMCounter.value) := True
              }
            }.otherwise{
              starterIds.foreach { j =>
                val data = (io.dataOuts(j).lsb ## outputBuffers(j)(w - 1 downto 1)).asUInt
                // design: selective write
                when(writeProduct)(productLasts(j + outputRAMCounter.value) := data)
                when(writeSquare)(squareLasts(j + outputRAMCounter.value) := data)
              }
            }
          }
        }
      }
    }

  }
}

object MontExpSystolic {
  def main(args: Array[String]): Unit = {

  }
}

