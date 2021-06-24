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
  val modeReg = Reg(HardType(io.mode))
  when(io.start)(modeReg := io.mode)

  val montMult = MontMulSystolicParallel(config)
  // pre-assign / set the controls
  montMult.io.start := False
  montMult.io.mode := Mux(io.start, io.mode, modeReg)
  // pre-assign the data inputs
  Seq(montMult.io.xiIns, montMult.io.MWordIns, montMult.io.YWordIns).foreach(_.foreach(_.clearAll()))
  montMult.io.YWordIns.foreach(_.allowOverride)

  // MEMORIES
  // these three RAMs are for secret-key related data, and will be kept for the whole lifecycle of a MontExp
  val Seq(rSquareWordRAM, mWordRAM, exponentWordRAM) =
  Seq(rSquare, M, BigInt(E.toString(2).reverse, 2)).map(bigint => Mem(toWords(bigint, w, lMs.max / w).map(U(_, w bits))))
  // these two RAMs are for partial results generated through the MontExp procedure
  val xWords = Xs.map(x => toWords(x, w, wordPerGroup))
  // to store the Montgomery representation of x, which is x \times r^{-1} \pmod M
  val xMontRAMs = Seq.fill(parallelFactor)(Mem(UInt(w bits), wordPerGroup))
  // to store the partial product of the MontExp, which is x at the beginning and x^{e} \pmod M
  val productRAMs = xWords.map(XWord => Mem(XWord.map(U(_, w bits)))) // at the beginning, x^0 = x
  // for last, irregular part of the two RAMs above
  val xMontLasts, productLasts = Seq.fill(parallelFactor)(RegInit(U(0, w bits)))

  // INPUT COUNTERS
  // counters are the main controllers of the input scheme
  // generally, the input are controlled by three different "rhythms"
  //  1. X is fed bit by bit
  //  2. Y/M are fed word by word, specially, montMult has an inner counter(eCounter) for Y/M, we save that
  //  3. the next exponent bit should be fetched when a MontMult is done
  // all these counters are triggered by corresponding "need feed" signals, who take effect in the fsm part
  // starters
//  val xCounter = MultiCountCounter(groupPerInstance.map(BigInt(_)), modeReg, inc = xWordCounter.willOverflow)
  val xBitCounter, exponentBitCounter = Counter(w)
  val outputWordCounter = Counter(wordPerGroup)
  // cascaded counters driven by starters
  val xWordCounter = Counter(wordPerGroup, inc = xBitCounter.willOverflow)
  println(s"RAMCounter counts = ${lMs.map(lM => BigInt(lM / lMs.min)).mkString(" ")}")
  // how many RAMs should be involved in an instance
  val xRAMCounter = MultiCountCounter(groupPerInstance.map(BigInt(_)), modeReg, inc = xWordCounter.willOverflow)
  val outputRAMCounter = MultiCountCounter(groupPerInstance.map(BigInt(_)), modeReg, inc = outputWordCounter.willOverflow)
  val exponentWordCounter = MultiCountCounter(lMs.map(lM => BigInt(lM / w)), modeReg, inc = exponentBitCounter.willOverflow)
  val exponentLengthReg = RegInit(U(ELength))

  val exponentCurrentBit = exponentWordRAM(exponentWordCounter.value)(exponentBitCounter.value)
  val lastExponentBit = (exponentWordCounter @@ exponentBitCounter) === (exponentLengthReg - 1)

  def readRAMsBit(rams: Seq[Mem[UInt]], wordId: UInt, bitId: UInt) = Vec(rams.map(ram => ram(wordId)(bitId)))
  def readRAMsWord(rams: Seq[Mem[UInt]], wordId: UInt) = Vec(rams.map(ram => ram(wordId)))

  // control the write back behavior
  val writeProduct = RegInit(False)
  val writeXMont = RegInit(False)
  val outputBuffers = montMult.io.dataOuts.map(RegNext(_)) // delay t for shift
  val shiftedOutputs = Vec(montMult.io.dataOuts.zip(outputBuffers).map { case (prev, next) => (prev.lsb ## next(w - 1 downto 1)).asUInt })
  val validNext = RegNext(montMult.io.valids(0)) // delay valid for one cycle
  validNext.init(False)
  val datasToWrite = Vec(UInt(w bits), parallelFactor)
  datasToWrite.foreach(_.clearAll())
  val outputRAMEnables = Vec(Bool, parallelFactor)
  outputRAMEnables.foreach(_.clear())
  val writeBackLastWord = RegNext(outputRAMCounter.willOverflow)
  writeBackLastWord.init(False)
  (0 until parallelFactor).foreach { i =>
    xMontRAMs(i).write(
      address = outputWordCounter.value,
      data = datasToWrite(i),
      enable = outputRAMEnables(i) && writeXMont && validNext
    )
    productRAMs(i).write(
      address = outputWordCounter.value,
      data = datasToWrite(i),
      enable = outputRAMEnables(i) && writeProduct && validNext
    )
  }

  val tobeValid = RegInit(False)
  io.dataOuts := shiftedOutputs
  io.valids.zip(montMult.io.valids).foreach { case (io, mult) =>
    io := RegNext(mult) && tobeValid
  }
  when(io.valids(0).fall())(tobeValid.clear())

  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val PRE, MULT, SQUARE, POST = new State()
    val RUNs = Seq(PRE, MULT, SQUARE, POST)

    def startMontMult() = montMult.io.start.set()

    IDLE.whenIsActive {
      when(io.start) {
        goto(PRE)
        montMult.io.start.set()
      }
    }

    // state transition
    PRE.whenIsActive(when(montMult.fsm.lastCycle)(goto(SQUARE)))

    SQUARE.whenIsActive(when(montMult.fsm.lastCycle) {
      when(exponentCurrentBit)(goto(MULT)) // when current bit is 1, always goto MULT for a multiplication
        .elsewhen(lastExponentBit) {
          goto(POST)
          exponentBitCounter.clear()
          exponentWordCounter.clear()
        }
        .otherwise(goto(SQUARE))
    })
    MULT.whenIsActive(when(montMult.fsm.lastCycle) {
      when(lastExponentBit) {
        goto(POST)
        exponentBitCounter.clear()
        exponentWordCounter.clear()
      }
        .otherwise(goto(SQUARE))
    })
    POST.whenIsActive(when(montMult.fsm.lastCycle)(goto(IDLE)))

    SQUARE.whenIsNext(when(montMult.fsm.lastCycle)(exponentBitCounter.increment()))

    when(montMult.fsm.lastCycle) { // these four states end when a MontMul task ends
      when(Seq(PRE, MULT, SQUARE).map(isActive(_)).xorR)(montMult.io.start := True)
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
              xCandidates := readRAMsBit(productRAMs, xWordCounter, xBitCounter)
            }.otherwise {
              xCandidates := Vec(productLasts.map(word => word(xBitCounter.value)))
            }
            starterIds.foreach(j => montMult.io.xiIns(j) := xCandidates(xRAMCounter.value + j).asUInt) // feed X
          }
          // feed Y and Modulus
          when(montMult.fsm.feedMYNow) {
            val yCandidates = Vec(UInt(w bits), parallelFactor) // prepare for different modes, for each instance

            val ymCount = montMult.fsm.MYWordIndex // following logics are valid with the requirement that w is a power of 2
            val wordAddrLength = xMontRAMs(0).addressWidth
            val ymRAMCount = ymCount.splitAt(wordAddrLength)._1.asUInt // higher part- the RAM count
            val ymWordCount = ymCount.splitAt(wordAddrLength)._2.asUInt // lower part - the word count

            yCandidates.foreach(_.clearAll()) // pre - assignment
            def push0() = yCandidates := Vec(Seq.fill(parallelFactor)(U(0, w bits)))
            def push1() = yCandidates := Vec(Seq.fill(parallelFactor)(U(1, w bits)))

            when(!montMult.fsm.lastWord) {
              montMult.io.MWordIns.foreach(_ := mWordRAM(ymCount)) // feed M
              // only for RSA, as the true word number is a power of 2
              when(isActive(PRE))(yCandidates := Vec(Seq.fill(parallelFactor)(rSquareWordRAM(ymCount)))) // feed Y
                .elsewhen(isActive(MULT))(yCandidates := readRAMsWord(xMontRAMs, ymWordCount))
                .elsewhen(isActive(SQUARE))(yCandidates := readRAMsWord(productRAMs, ymWordCount))
                .elsewhen(isActive(POST)) { // feed 1
                  when(ymWordCount === U(0))(push1())
                    .otherwise(push0())
                }
            }.otherwise {
              starterIds.foreach(j => montMult.io.YWordIns(j) := U(0, w bits)) // feed Y
              when(isActive(PRE))(push0()) // msw = 0
                .elsewhen(isActive(MULT))(yCandidates := Vec(xMontLasts))
                .elsewhen(isActive(SQUARE))(yCandidates := Vec(productLasts))
                .elsewhen(isActive(POST))(push0()) // msw = 0
            }
            starterIds.foreach(j => montMult.io.YWordIns(j) := yCandidates(ymRAMCount + j))
          }
          // fetch XYR^-1 and write back
          when(montMult.fsm.lastRound) {
            when(isActive(PRE)) {
              writeXMont.set()
              writeProduct.set()
            }.elsewhen(isActive(SQUARE) || isActive(MULT) || isActive(POST)) { // TODO: result after post is not needed
              writeXMont.clear()
              writeProduct.set()
            }.otherwise {
              writeXMont.clear()
              writeProduct.clear()
            }
            when(isActive(POST))(tobeValid.set())
          }
          when(validNext) {
            when(!writeBackLastWord) {
              outputWordCounter.increment()
              starterIds.foreach { j =>
                val data = (montMult.io.dataOuts(j).lsb ## outputBuffers(j)(w - 1 downto 1)).asUInt
                // design: selective write
                datasToWrite(j + outputRAMCounter.value) := data
                outputRAMEnables(j + outputRAMCounter.value) := True
              }
            }.otherwise {
              starterIds.foreach { j =>
                val data = (io.dataOuts(j).lsb ## outputBuffers(j)(w - 1 downto 1)).asUInt
                // design: selective write
                when(writeProduct)(productLasts(j + outputRAMCounter.value) := data)
                when(writeXMont)(xMontLasts(j + outputRAMCounter.value) := data)
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

