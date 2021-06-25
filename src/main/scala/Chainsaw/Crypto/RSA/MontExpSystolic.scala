package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class MontExpSystolic(config: MontConfig) extends Component {

  import config._

  // TODO: test for closely continuous workload
  require(isPow2(w) && isPow2(lMs.min))

  val io = new Bundle {
    val start = in Bool()
    val mode = in Bits (lMs.size bits)
    val keyReset = in Bool()
    val xWordIns = in Vec(UInt(w bits), parallelFactor)
    val modulusWordIn, radixSquareWordIn, exponentWordIn = in UInt (w bits) // Y for the first MontMul
    val exponentLengthIn = in UInt (log2Up(lMs.max + 1) bits)
    val dataOuts = out Vec(UInt(w bits), parallelFactor)
    val valids = out Vec(Bool, parallelFactor)
  }
  val modeReg = RegNextWhen(io.mode, io.start, init = B"00000")
  val keyResetReg = RegNextWhen(io.keyReset, io.start, init = False)
  val exponentLengthReg = RegNextWhen(io.exponentLengthIn, io.start, init = U(0))


  // BLOCK OPERATOR
  val montMult = MontMulSystolicParallel(config)
  // pre-assign / set the controls
  montMult.io.start := False
  montMult.io.mode := Mux(io.start, io.mode, modeReg)
  // pre-assign the data inputs
  Seq(montMult.io.xiIns, montMult.io.MWordIns, montMult.io.YWordIns).foreach(_.foreach(_.clearAll()))
  montMult.io.YWordIns.foreach(_.allowOverride)

  // BLOCK MEMORIES
  // these three RAMs are for secret-key related data, and will be kept for the whole lifecycle of a MontExp
  //  val Seq(radixSquareWordRAM, modulusWordRAM, exponentWordRAM) =
  //  Seq(rSquare, M, BigInt(E.toString(2).reverse, 2)).map(bigint => Mem(toWords(bigint, w, lMs.max / w).map(U(_, w bits))))
  // these two RAMs are for partial results generated through the MontExp procedure
  val radixSquareWordRAM, modulusWordRAM, exponentWordRAM = Mem(UInt(w bits), lMs.max / w)
  // to store the Montgomery representation of x, which is x \times r^{-1} \pmod M
  val xMontRAMs = Seq.fill(parallelFactor)(Mem(UInt(w bits), wordPerGroup))
  // to store the partial product of the MontExp, which is x at the beginning and x^{e} \pmod M
  //  val productRAMs = xWords.map(XWord => Mem(XWord.map(U(_, w bits)))) // at the beginning, x^0 = x
  val productRAMs = Seq.fill(parallelFactor)(Mem(UInt(w bits), wordPerGroup))
  // for last, irregular part of the two RAMs above
  val xMontLasts, productLasts = Seq.fill(parallelFactor)(RegInit(U(0, w bits)))
  // register storing the length of exponent
  //  val exponentLengthReg = RegInit(U(ELength))

  // BLOCK DATAPATH FOR INPUT SCHEME
  // counters are the main controllers of the input scheme
  // generally, the input are controlled by three different "rhythms"
  //  0. when init, all inputs from the "outside world" are word by word
  //  1. X is fed bit by bit
  //  2. Y/M are fed word by word, specially, montMult has an inner counter(eCounter) for Y/M, we save that
  //  3. the next exponent bit should be fetched when a MontMult is done
  // all these counters are triggered by corresponding "need feed" signals, who take effect in the fsm part
  // as word size and smallest RSA size(lM.min = 512) are powers of 2, take the bit/word/RAM addr by bit select
  val counterLengths = Seq(w, wordPerGroup, parallelFactor).map(log2Up(_))
  val Seq(bitAddrLength, wordAddrLength, ramIndexLength) = counterLengths // when supporting 512-4096, they are 3,4,5
  val splitPoints = (0 until 4).map(i => counterLengths.take(i).sum) // when supporting 512-4096, 0,3,7,12

  val xCounter, exponentCounter = MultiCountCounter(lMs.map(BigInt(_)), modeReg)

  val xBitSelects = splitPoints.init.zip(splitPoints.tail).map { case (start, end) => xCounter.value(end - 1 downto start) } // 2 downto 0, 6 downto 3,...
  val Seq(xBitCount, xWordCount, xRAMCount) = xBitSelects

  val exponentBitSelect = splitPoints.init.zip(splitPoints.tail).map { case (start, end) => exponentCounter.value(end - 1 downto start) }
  val exponentBitCount = exponentBitSelect(0)
  val exponentWordCount = exponentBitSelect(1) @@ exponentBitSelect(2)
  val exponentCurrentBit = exponentWordRAM(exponentWordCount)(exponentBitCount)
  val exponentLastBit = exponentCounter.value === (exponentLengthReg - 1)

  val initCounter = MultiCountCounter(lMs.map(lM => BigInt(lM / w)), modeReg)
  val initRAMCount = initCounter(ramIndexLength + wordAddrLength - 1 downto wordAddrLength)
  val initWordCount = initCounter(wordAddrLength - 1 downto 0)

  // BLOCK DATAPATH FOR OUTPUT SCHEME
  // the output rhythm is word by word, and can also maintained by a counter
  // in fact, montMult has an inner counter(eCounter) maintains the word by word input / output rhythm, we reuse it
  // for RSA sizes, when e = lM / w + 1 and p = e - 1 = lM / w, n % p == 1, so outputCounter is two cycle delayed from MYWordIndex(output provider index + DtoQ delay)
  // besides, as the MontMult results need to be shifted left, on more cycle needed
  val outputCounter = Delay(montMult.fsm.MYWordIndex, 3)
  val outputRAMCount = outputCounter(ramIndexLength + wordAddrLength - 1 downto wordAddrLength)
  val outputWordCount = outputCounter(wordAddrLength - 1 downto 0)

  // also, for the output scheme, we need to "selectively write" RAMs, this is implemented by the enables of the RAMs
  val writeProduct, writeXMont = RegInit(False) // select RAM group
  Seq(writeProduct, writeXMont).foreach(_.allowOverride)
  val montMultOutputValid = montMult.io.valids(0)
  val shiftedOutputValid = RegNext(montMultOutputValid) // delay valid for one cycle as we need to shift it
  shiftedOutputValid.init(False)
  val ramEnables = Vec(Bool, parallelFactor) // select RAM
  ramEnables.foreach(_.clear())
  val addrToWrite = UInt(wordAddrLength bits)
  addrToWrite.clearAll()
  addrToWrite.allowOverride
  val dataToWrite = Vec(UInt(w bits), parallelFactor)
  dataToWrite.foreach(_.clearAll())
  (0 until parallelFactor).foreach { i => // the selectively, synchronous write ports
    xMontRAMs(i).write(
      address = addrToWrite,
      data = dataToWrite(i),
      enable = ramEnables(i) && writeXMont // the only difference
    )
    productRAMs(i).write(
      address = addrToWrite,
      data = dataToWrite(i),
      enable = ramEnables(i) && writeProduct
    )
  }

  // get the shifted output
  val outputBuffers = montMult.io.dataOuts.map(RegNext(_)) // delay t for shift
  val shiftedMontMultOutputs = Vec(montMult.io.dataOuts.zip(outputBuffers).map { case (prev, next) => (prev.lsb ## next(w - 1 downto 1)).asUInt })

  // define the final output of MontExp
  val tobeValid = RegInit(False) // this will be set at the last round of POST by the fsm
  when(io.valids(0).fall())(tobeValid.clear()) // and cleared when the continuous output ends, then wait for next POST
  io.dataOuts := shiftedMontMultOutputs
  // output is valid, when montMult output is valid and now is the last MontMult operation
  io.valids.zip(montMult.io.valids).foreach { case (io, mult) => io := RegNext(mult) && tobeValid }

  val fsm = new StateMachine { // BLOCK STATE MACHINE, which controls the datapath defined above
    val IDLE = StateEntryPoint()
    val INIT, PRE, MULT, SQUARE, POST = new State()
    val RUNs = Seq(PRE, MULT, SQUARE, POST)

    // for readability, we pre-define some "task" at the beginning
    def readRAMsBit(rams: Seq[Mem[UInt]], wordId: UInt, bitId: UInt) = Vec(rams.map(ram => ram(wordId)(bitId)))
    def readRAMsWord(rams: Seq[Mem[UInt]], wordId: UInt) = Vec(rams.map(ram => ram(wordId)))
    def initOver = initCounter.willOverflow
    def startMontMult() = montMult.io.start.set()
    def montMultOver = montMult.fsm.lastCycle
    def lastMontMult = exponentLastBit

    // generally speaking, we have three states: IDLE, INIT, and RUN
    //  at INIT, the user provides the inputs(Xs) and optionally reset the secret key(provides modulus, exponent and precomputed radixSquare)
    //  at RUN, we do MontMult operations one by one, following a specific order, and get operands from different RAMs
    //  according to the source of the operands, we have PRE, POST, MULT and SQUARE
    // part 1: state transitions
    //  by the way control montMult starting points and exponent bit traversing, as they happens together with state transitions
    // TODO: after INIT added, these should be modified
    // BLOCK STATE TRANSITION
    //    IDLE.whenIsActive(when(io.start)(goto(PRE)))
    IDLE.whenIsActive(when(io.start)(goto(INIT)))
    INIT.whenIsActive(when(initOver)(goto(PRE)))
    PRE.whenIsActive(when(montMultOver)(goto(SQUARE)))
    SQUARE.whenIsActive(when(montMultOver) {
      when(exponentCurrentBit)(goto(MULT)) // when current bit is 1, always goto MULT for a multiplication
        .elsewhen(lastMontMult)(goto(POST)) // current bit is 0 and no next bit, goto POST
        .otherwise(goto(SQUARE)) // current bit is 0 and has next bit, goto SQUARE
    })
    MULT.whenIsActive(when(montMultOver) {
      when(lastMontMult)(goto(POST))
        .otherwise(goto(SQUARE))
    })
    POST.whenIsActive(when(montMultOver) {
      when(io.start)(goto(INIT))
        .otherwise(goto(IDLE))
    })
    //
    INIT.onExit(startMontMult())
    Seq(PRE, SQUARE, MULT).foreach(_.whenIsActive(when(montMultOver)(startMontMult())))
    // SQUARE is the first operation for each exponent bit in L2R order, so exponent bit iterates whenever a new SQUARE will be entered
    SQUARE.whenIsNext(when(montMultOver)(exponentCounter.increment()))
    POST.onEntry(exponentCounter.clear())

    // part 2: state workload
    // the workload of INIT is to store the input provided
    // and the workload of RUN(PRE, POST, SQUARE, MULT) is to feed data to montMult, and write back the result from montMult to the RAMs
    //  particularly, in our memory system, the last words, as they are irregular should always be written/read into/from the "last word regs",
    //  rather than the regular RAMs
    switch(True) { // for different modes
      lMs.indices.foreach { i => // traverse each mode, as each mode run instances of different size lM
        val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(i) == 0) // instance indices of current mode
            .take(parallelFactor / groupPerInstance(i))
        is(modeReg(i)) { // for each mode
          IDLE.onExit { // preset the write flag for INIT
            writeXMont.clear()
            writeProduct.set()
          }
          when(isActive(INIT)) { // BLOCK workload0: store the input for MontExp
            initCounter.increment()
            starterIds.foreach { j => // BLOCK workload0.0: always, writing Xs into productRAMs
              ramEnables(j + initRAMCount).set() // select RAMs
              addrToWrite := initWordCount
              dataToWrite(j + initRAMCount) := io.xWordIns(j)
            }
            when(keyResetReg) { // BLOCK workload0.1: when resetting secret key,
              modulusWordRAM(initCounter.value) := io.modulusWordIn
              radixSquareWordRAM(initCounter.value) := io.radixSquareWordIn
              exponentWordRAM(initCounter.value) := io.exponentWordIn
            }
          }
          when(montMult.fsm.feedXNow) { // BLOCK workload1: feed X, for every stage, X is from the productRAMs
            when(!montMult.fsm.lastRound)(xCounter.increment())
            val xLast = Vec(productLasts.map(word => word(xBitCount)))
            val xInit = readRAMsBit(productRAMs, xWordCount, xBitCount)
            val xCandidates = Mux(montMult.fsm.lastRound, xLast, xInit)
            starterIds.foreach(j => montMult.io.xiIns(j) := xCandidates(xRAMCount + j).asUInt) // feed X, according to current mode
          }

          when(montMult.fsm.feedMYNow) { // BLOCK workload2: feed Y and Modulus
            val ymCount = montMult.fsm.MYWordIndex // following logics are valid with the requirement that w is a power of 2
            val wordAddrLength = xMontRAMs(0).addressWidth
            val ymRAMCount = ymCount.splitAt(wordAddrLength)._1.asUInt // higher part- the RAM count
            val ymWordCount = ymCount.splitAt(wordAddrLength)._2.asUInt // lower part - the word count

            val yCandidates = Vec(UInt(w bits), parallelFactor) // prepare for different modes, for each instance
            yCandidates.foreach(_.clearAll()) // pre - assignment
            def feed0() = yCandidates := Vec(Seq.fill(parallelFactor)(U(0, w bits)))
            def feed1() = yCandidates := Vec(Seq.fill(parallelFactor)(U(1, w bits)))

            when(!montMult.fsm.lastWord)(montMult.io.MWordIns.foreach(_ := modulusWordRAM(ymCount))) // M is from the modulusRAM
            when(!montMult.fsm.lastWord) { // Y is from different RAMs at different stage
              // only for RSA, as the true word number is a power of 2
              when(isActive(PRE))(yCandidates := Vec(Seq.fill(parallelFactor)(radixSquareWordRAM(ymCount)))) // from radixSquare
                .elsewhen(isActive(MULT))(yCandidates := readRAMsWord(xMontRAMs, ymWordCount)) // from xMont
                .elsewhen(isActive(SQUARE))(yCandidates := readRAMsWord(productRAMs, ymWordCount)) // from partialProduct
                .elsewhen(isActive(POST)) { // 1, as POST executes MontMult(x^e', 1)
                  when(ymCount === U(0))(feed1())
                    .otherwise(feed0())
                }
              // need no default, as yCandidates have been pre-assigned
            }.otherwise { // a this time, the most significant word(msw) of Y should be fed
              starterIds.foreach(j => montMult.io.YWordIns(j) := U(0, w bits)) // feed Y
              when(isActive(PRE))(feed0()) // msw = 0
                .elsewhen(isActive(MULT))(yCandidates := Vec(xMontLasts))
                .elsewhen(isActive(SQUARE))(yCandidates := Vec(productLasts))
                .elsewhen(isActive(POST))(feed0()) // msw = 0
            }
            starterIds.foreach(j => montMult.io.YWordIns(j) := yCandidates(ymRAMCount + j))
          }
          // BLOCK workload3: fetch XYR^-1 \pmod M and write back
          when(montMult.fsm.lastRound) {
            // as we have defined the write back datapath above, we control write back behavior by
            //  1. setting proper triggers
            //  2. set "dataToWrite" with proper data
            // the trigger "lastRound" is just before the valids, and inside the state
            // valids themselves can't be the triggers as they last even when the state has changed(the "trailing" phenomenon of the output)
            when(isActive(PRE)) { // BLOCK workload3.0: determine which group of RAMs should be written
              writeXMont.set()
              writeProduct.set()
            }.elsewhen(isActive(SQUARE) || isActive(MULT) || isActive(POST)) { // TODO: result after post is not needed
              writeXMont.clear()
              writeProduct.set()
            }.otherwise {
              writeXMont.clear()
              writeProduct.clear()
            }
            // similarly, we mark the final output by setting a trigger which last long enough to cooperate with the trailing valids
            when(isActive(POST))(tobeValid.set())
          }
          when(shiftedOutputValid) { // BLOCK workload3.1: set "dataToWrite" with proper data
            val writeBackLastWord = montMult.io.valids(0).fall() // the last word flag
            starterIds.foreach { j =>
              when(!writeBackLastWord) {
                ramEnables(j + outputRAMCount).set() // select RAMs
                addrToWrite := outputWordCount
                dataToWrite(j + outputRAMCount) := shiftedMontMultOutputs(j)
              }.otherwise {
                when(writeProduct)(productLasts(j + outputRAMCount) := shiftedMontMultOutputs(j))
                when(writeXMont)(xMontLasts(j + outputRAMCount) := shiftedMontMultOutputs(j))
              }
            }
          }
        }
      }
    }
  }

  val debug = new Area {
    val isINIT = fsm.isActive(fsm.INIT)
    isINIT.simPublic()
    isINIT.allowPruning()
    exponentWordRAM.simPublic()
    modulusWordRAM.simPublic()
    radixSquareWordRAM.simPublic()
  }

}

object MontExpSystolic {
  def main(args: Array[String]): Unit = {
  }
}

