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

/**
 * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 Chainsaw RSA]]
 */
case class MontExpSystolic(config: MontConfig) extends Component {

  import config._

  // TODO: test for closely continuous workload
  // TODO: optimize the selection network
  // TODO: implement the final reduction
  // TODO: clean up the design, draw a new graph
  // TODO: improve fmax
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
  val modeReg = RegNextWhen(io.mode, io.start, init = B"00000") // these states are set when io.start is valid
  val keyResetReg = RegNextWhen(io.keyReset, io.start, init = False)
  val exponentLengthReg = RegNextWhen(io.exponentLengthIn, io.start, init = U(0))

  // BLOCK: OPERATOR
  val montMult = MontMulSystolicParallel(config)
  montMult.io.start.clear() // pre-assign / set the controls
  montMult.io.mode := Mux(io.start, io.mode, modeReg)
  Seq(montMult.io.xiIns, montMult.io.MWordIns, montMult.io.YWordIns).foreach(_.foreach(_.clearAll())) // pre-assign the data inputs X,Y,M

  // BLOCK: MEMORIES
  /**
   * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 "Memories" in this page]]
   */
  val radixSquareWordRAM, modulusWordRAM, exponentWordRAM = Mem(UInt(w bits), lMs.max / w)
  val xMontRAMs, productRAMs = Seq.fill(parallelFactor)(Mem(UInt(w bits), wordPerGroup))
  val xMontLasts, productLasts = Vec(Seq.fill(parallelFactor)(RegInit(U(0, w bits))))


  // BLOCK DATAPATH FOR INPUT/OUTPUT SCHEME
  /**
   * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 "Input/output scheme" in this page]]
   */
  // BLOCK COUNTERS
  val counterLengths = Seq(w, wordPerGroup, parallelFactor).map(log2Up(_))
  val Seq(bitAddrLength, wordAddrLength, ramIndexLength) = counterLengths // 3,4,5
  val splitPoints = (0 until 4).map(i => counterLengths.take(i).sum) // 0,3,7,12

  val initCounter = MultiCountCounter(lMs.map(lM => BigInt(lM / w)), modeReg)
  val initRAMCount = initCounter.splitAt(wordAddrLength)._1.asUInt
  val initWordCount = initCounter.splitAt(wordAddrLength)._2.asUInt

  val xCounter, exponentCounter = MultiCountCounter(lMs.map(BigInt(_)), modeReg)

  val xBitSelects = splitPoints.init.zip(splitPoints.tail).map { case (start, end) => xCounter.value(end - 1 downto start) } // 2 downto 0, 6 downto 3,...
  val Seq(xBitCount, xWordCount, xRAMCount) = xBitSelects

  val exponentBitSelect = splitPoints.init.zip(splitPoints.tail).map { case (start, end) => exponentCounter.value(end - 1 downto start) }
  val exponentBitCount = exponentBitSelect(0)
  val exponentWordCount = exponentBitSelect(1) @@ exponentBitSelect(2)
  val currentExponentBit = exponentWordRAM(exponentWordCount)(exponentBitCount)
  val lastExponentBit = exponentCounter.value === (exponentLengthReg - 1)

  val ymCount = montMult.fsm.MYWordIndex // following logics are valid with the requirement that w is a power of 2
  val ymRAMCount = ymCount.splitAt(wordAddrLength)._1.asUInt // higher part- the RAM count
  val ymWordCount = ymCount.splitAt(wordAddrLength)._2.asUInt // lower part - the word count

  val outputCounter = Delay(montMult.fsm.MYWordIndex, 3)
  val outputRAMCount = outputCounter.splitAt(wordAddrLength)._1.asUInt
  val outputWordCount = outputCounter.splitAt(wordAddrLength)._2.asUInt

  // BLOCK MEMORY PORTS
  val writeProduct, writeXMont = RegInit(False) // select RAM group
  Seq(writeProduct, writeXMont).foreach(_.allowOverride)
  // ports writing xMontRAMs and productRAMs
  val ramEnables = Vec(Bool, parallelFactor) // select RAM
  val addrToWrite = UInt(wordAddrLength bits)
  val dataToWrite = Vec(UInt(w bits), parallelFactor)
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
  ramEnables.foreach(_.clear()) // pre-assignments
  addrToWrite.clearAll()
  dataToWrite.foreach(_.clearAll())
  addrToWrite.allowOverride

  // get the shifted output
  val outputBuffers = montMult.io.dataOuts.map(RegNext(_)) // delay t for shift
  val shiftedMontMultOutputs = Vec(montMult.io.dataOuts.zip(outputBuffers).map { case (prev, next) => (prev.lsb ## next(w - 1 downto 1)).asUInt })
  val montMultOutputValid = montMult.io.valids(0)
  val shiftedOutputValid = RegNext(montMultOutputValid, init = False) // delay valid for one cycle as we need to shift it

  // define the final output of MontExp
  val tobeValid = RegInit(False) // this will be set at the last round of POST by the fsm
  when(io.valids(0).fall())(tobeValid.clear()) // and cleared when the continuous output ends, then wait for next POST
  io.dataOuts := shiftedMontMultOutputs
  // output is valid, when montMult output is valid and now is the last MontMult operation
  io.valids.zip(montMult.io.valids).foreach { case (io, mult) => io := RegNext(mult) && tobeValid }

  val fsm = new StateMachine { // BLOCK STATE MACHINE, which controls the datapath defined above
    // BLOCK STATE DECLARATION
    val IDLE = StateEntryPoint()
    val INIT, PRE, MULT, SQUARE, POST = new State()
    // BLOCK STATE TRANSITION
    // for readability, we pre-define some "task" at the beginning
    def initOver = initCounter.willOverflow
    def startMontMult() = montMult.io.start.set()
    def montMultOver = montMult.fsm.lastCycle
    def lastMontMult = lastExponentBit
    def lastRound = montMult.fsm.lastRound
    def firstWord = ymCount === U(0)
    def lastWord = montMult.fsm.lastWord
    def feedX = montMult.fsm.feedXNow
    def feedYM = montMult.fsm.feedMYNow
    IDLE.whenIsActive(when(io.start)(goto(INIT)))
    INIT.whenIsActive(when(initOver)(goto(PRE)))
    PRE.whenIsActive(when(montMultOver)(goto(SQUARE)))
    SQUARE.whenIsActive(when(montMultOver) {
      when(currentExponentBit)(goto(MULT)) // when current bit is 1, always goto MULT for a multiplication
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
    INIT.onExit(startMontMult())
    Seq(PRE, SQUARE, MULT).foreach(_.whenIsActive(when(montMultOver)(startMontMult()))) // control the montMult module
    SQUARE.whenIsNext(when(montMultOver)(exponentCounter.increment())) // SQUARE is the first operation for each exponent bit, so exponent moves
    POST.onEntry(exponentCounter.clear())
    // BLOCK STATE WORKLOAD
    val routerToRAMs = MontExpRouter12N(config, UInt(w bits))
    routerToRAMs.src.foreach(_.clearAll())
    routerToRAMs.selectCount.clearAll()

    // BLOCK workload0: store the input for MontExp
    val writeBackLastWord = montMult.io.valids(0).fall() // the last word flag
    when(isActive(INIT)) { // BLOCK workload0.0: always, writing Xs into productRAMs
      initCounter.increment()
      routerToRAMs.src := io.xWordIns
      routerToRAMs.selectCount := initRAMCount
      addrToWrite := initWordCount
    }.elsewhen(shiftedOutputValid && !tobeValid) { // BLOCK workload3.1: set "dataToWrite" with proper data and "open" the RAMs
      routerToRAMs.src := shiftedMontMultOutputs
      routerToRAMs.selectCount := outputRAMCount
      addrToWrite := outputWordCount
    }

    routerToRAMs.work := True
    routerToRAMs.mode := modeReg
    dataToWrite.allowOverride
    dataToWrite := routerToRAMs.toDes

    /**
     * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 "Flows" in this page]]
     */
    // BLOCK flow0
    val secretKeyRAMs = Seq(modulusWordRAM, radixSquareWordRAM, exponentWordRAM)
    val secretKeyInputs = Seq(io.modulusWordIn, io.radixSquareWordIn, io.exponentWordIn)
    secretKeyRAMs.zip(secretKeyInputs).foreach { case (ram, io) => ram.write(initCounter.value, io, enable = isActive(INIT) && keyResetReg) }
    // BLOCK flow1 & 7
    // dual-port, as when SQUARE is active, productRAMs are read concurrently in two different manners
    val productWordsForX = Mux(!lastRound, Vec(productRAMs.map(ram => ram.readAsync(xWordCount))), productLasts)
    val productBitsForX = Vec(productWordsForX.map(word => word(xBitCount).asUInt))
    when(feedX)(when(!lastRound)(xCounter.increment()))

    val routerProdToX = MontExpRouterN21(config, UInt(1 bits))
    routerProdToX.work := montMult.fsm.feedXNow
    routerProdToX.mode := modeReg
    routerProdToX.selectCount := xRAMCount
    routerProdToX.src := productBitsForX
    montMult.io.xiIns.allowOverride
    montMult.io.xiIns := routerProdToX.toDes

    // BLOCK flow3
    val productWordsForY = Mux(!lastWord, Vec(productRAMs.map(ram => ram.readAsync(ymWordCount))), productLasts)
    val xMontWordsForY = Mux(!lastWord, Vec(xMontRAMs.map(ram => ram.readAsync(ymWordCount))), xMontLasts)
    val radixSquareWordForY = Mux(!lastWord, radixSquareWordRAM.readAsync(ymCount), U(0, w bits))

    val wordsForY = Vec(UInt(w bits), parallelFactor)
    wordsForY.foreach(_.clearAll())
    def feed0() = wordsForY := Vec(Seq.fill(parallelFactor)(U(0, w bits)))
    def feed1() = wordsForY := Vec(Seq.fill(parallelFactor)(U(1, w bits)))
    when(isActive(PRE))(wordsForY := Vec(Seq.fill(parallelFactor)(radixSquareWordForY))) // from radixSquare
      .elsewhen(isActive(MULT))(wordsForY := xMontWordsForY) // from xMont // selection network version
      .elsewhen(isActive(SQUARE))(wordsForY := productWordsForY) // from partialProduct // selection network version
      .elsewhen(isActive(POST)) { // 1, as POST executes MontMult(x^e', 1)
        when(!lastWord) {
          when(firstWord)(feed1()).otherwise(feed0())
        }.otherwise(feed0())
      }

    // BLOCK workload2: feed Y and Modulus
    val routerToY = MontExpRouterN21(config, UInt(w bits))
    routerToY.work := montMult.fsm.feedMYNow
    routerToY.mode := modeReg
    routerToY.selectCount := ymRAMCount
    routerToY.src := wordsForY
    montMult.io.YWordIns.allowOverride
    montMult.io.YWordIns := routerToY.toDes

    val modulusWordForM = Mux(!montMult.fsm.lastWord, modulusWordRAM.readAsync(ymCount), U(0, w bits))
    when(montMult.fsm.feedMYNow)(montMult.io.MWordIns.foreach(_ := modulusWordForM)) // M is from the modulusRAM)

    switch(True) { // for different modes
      lMs.indices.foreach { modeId => // traverse each mode, as each mode run instances of different size lM
        val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(modeId) == 0) // instance indices of current mode
          .take(parallelFactor / groupPerInstance(modeId))
        is(modeReg(modeId)) { // for each mode
          IDLE.onExit { // preset the write flag for INIT
            writeXMont.clear()
            writeProduct.set()
          }

          when(isActive(INIT)) { // BLOCK workload0: store the input for MontExp
            initCounter.increment()
            starterIds.foreach { j => // BLOCK workload0.0: always, writing Xs into productRAMs
              ramEnables(j + initRAMCount).set() // select RAMs
            }
          }

          // BLOCK workload3: fetch XYR^-1 \pmod M and write back
          when(montMult.fsm.lastRound) {
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
          // BLOCK workload3.1: set "dataToWrite" with proper data and "open" the RAMs
          //  particularly, no write back for the last MontMult(final result), and the router works for it INIT workload
          when(shiftedOutputValid && !tobeValid) {
            starterIds.foreach { j =>
              when(!writeBackLastWord) {
                ramEnables(j + outputRAMCount).set() // select RAMs
              }.otherwise {
                when(writeProduct)(productLasts := dataToWrite)
                when(writeXMont)(xMontLasts := dataToWrite)
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
  }
}

object MontExpSystolic {
  def main(args: Array[String]): Unit = {
  }
}

