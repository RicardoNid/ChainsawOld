package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

/**
 * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 Chainsaw RSA]]
 */
case class MontExpSystolic(config: MontConfig) extends Component {

  import config._

  // TODO: test for closely continuous workload
  // TODO: optimize the selection network
  // TODO: implement the final reduction
  // TODO: clean up the design, draw some new graphs
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
  montMult.io.run.clear() // pre-assign / set the controls
  montMult.io.run.allowOverride // pre-assign / set the controls
  montMult.io.mode := Mux(io.start, io.mode, modeReg)
  Seq(montMult.io.xiIns, montMult.io.MWordIns, montMult.io.YWordIns).foreach { dataIn => // pre-assign the data inputs X,Y,M
    dataIn.foreach(_.clearAll())
    dataIn.allowOverride
  }

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
  val Seq(bitAddrLength, wordAddrLength, ramIndexLength) = counterLengths // 5,4,3
  val splitPoints = (0 until 4).map(i => counterLengths.take(i).sum) // 0,5,9,12

  val initCounter = MultiCountCounter(lMs.map(lM => BigInt(lM / w)), modeReg)
  val initRAMCount = initCounter.splitAt(wordAddrLength)._1.asUInt
  val initWordCount = initCounter.splitAt(wordAddrLength)._2.asUInt

  val xCounter, exponentCounter = MultiCountCounter(lMs.map(BigInt(_)), modeReg)

  val xBitSelects = splitPoints.init.zip(splitPoints.tail).map { case (start, end) => xCounter.value(end - 1 downto start) } // 4 downto 0, 8 downto 5,...
  val Seq(xBitCount, xWordCount, xRAMCount) = xBitSelects

  val exponentBitSelect = splitPoints.init.zip(splitPoints.tail).map { case (start, end) => exponentCounter.value(end - 1 downto start) }
  val exponentWordCount = exponentCounter.splitAt(bitAddrLength)._1.asUInt
  val exponentBitCount = exponentCounter.splitAt(bitAddrLength)._2.asUInt
  val currentExponentBit = exponentWordRAM(exponentWordCount)(exponentBitCount)
  val lastExponentBit = exponentCounter.value === (exponentLengthReg - 1)

  val ymCount = montMult.fsm.MYWordIndex // following logics are valid with the requirement that w is a power of 2
  val ymRAMCount = ymCount.splitAt(wordAddrLength)._1.asUInt // higher part- the RAM count
  val ymWordCount = ymCount.splitAt(wordAddrLength)._2.asUInt // lower part - the word count

  // TODO: make it behave correctly at "last"
  val wordMax = MuxOH(modeReg, lMs.map(lM => U(lM / w - 1, ymCount.getBitsWidth bits)))
  val delayedYmCount = Delay(ymCount, 3)
  val outputCounter = Mux(delayedYmCount > wordMax, U(0), delayedYmCount) // it's like a multicounter, but driven by the ymCount
  val outputRAMCount = outputCounter.splitAt(wordAddrLength)._1.asUInt
  val outputWordCount = outputCounter.splitAt(wordAddrLength)._2.asUInt

  // BLOCK MEMORY PORTS
  val writeXMont = RegInit(False) // select RAM group
  writeXMont.allowOverride
  // ports writing xMontRAMs and productRAMs
  val ramEnables = Vec(Bool, parallelFactor) // select RAM
  val addrToWrite = UInt(wordAddrLength bits)
  val dataToWrite = Vec(UInt(w bits), parallelFactor)
  (0 until parallelFactor).foreach { i => // the selectively, synchronous write ports
    xMontRAMs(i).write(
      address = addrToWrite,
      data = dataToWrite(i),
      enable = ramEnables(i) && writeXMont // extra condition for xMontRAMs
    )
    productRAMs(i).write(
      address = addrToWrite,
      data = dataToWrite(i),
      enable = ramEnables(i)
    )
  }
  addrToWrite.clearAll()
  addrToWrite.allowOverride

  val fsm = new StateMachine { // BLOCK STATE MACHINE, which controls the datapath defined above
    // BLOCK STATE DECLARATION
    val IDLE = StateEntryPoint()
    val INIT, PRE, MULT, SQUARE, POST = new State()
    // BLOCK STATE TRANSITION

    // for readability, we pre-define some "timing" at the beginning
    val initOver = initCounter.willOverflow
    def runMontMult() = montMult.io.run.set()
    val montMultOver = montMult.fsm.lastCycle
    val lastMontMult = lastExponentBit
    val lastRound = montMult.fsm.lastRound
    val firstWord = ymCount === U(0)
    val lastWord = montMult.fsm.lastWord
    val isFeedingX = montMult.fsm.feedXNow
    val isFeedingYM = montMult.fsm.feedMYNow
    val shouldWriteBack = isActive(PRE) || isActive(MULT) || isActive(SQUARE)
    val MontExpValid = RegInit(False) // will be implemented below
    val MontMultValids = RegNext(montMult.io.valids, init = montMult.io.valids.getZero) // delay valid for one cycle as we need to shift it
    val isWritingBack = MontMultValids(0) && !MontExpValid
    val writeBackLastWord = montMult.io.valids(0).fall()

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
    Seq(PRE, SQUARE, MULT, POST).foreach(_.whenIsActive(runMontMult())) // control the montMult module
    SQUARE.whenIsNext(when(montMultOver)(exponentCounter.increment())) // SQUARE is the first operation for each exponent bit, so exponent moves
    POST.onEntry(exponentCounter.clear())
    // BLOCK STATE WORKLOAD
    /**
     * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 "Flows" in this page]]
     */
    // BLOCK flow0
    val secretKeyRAMs = Seq(modulusWordRAM, radixSquareWordRAM, exponentWordRAM)
    val secretKeyInputs = Seq(io.modulusWordIn, io.radixSquareWordIn, io.exponentWordIn)
    secretKeyRAMs.zip(secretKeyInputs).foreach { case (ram, io) => ram.write(initCounter.value, io, enable = isActive(INIT) && keyResetReg) }
    // BLOCK flow1 & 7 & 8
    val outputBuffers = montMult.io.dataOuts.map(RegNext(_)) // delay t for shift
    val shiftedMontMultOutputs = Vec(montMult.io.dataOuts.zip(outputBuffers).map { case (prev, next) => (prev.lsb ## next(w - 1 downto 1)).asUInt })

    when(shouldWriteBack && lastRound) { // determine when should productRAMs and xMontRAMs be written
      when(isActive(PRE))(writeXMont.set())
        .otherwise(writeXMont.clear())
    }

    /**
     * @see [[https://www.notion.so/RSA-ECC-59bfcca42cd54253ad370defc199b090 "Routers" in this page]]
     */
    val routerToRAMEnables = MontExpRouter(config, Bits(1 bits), NTo1 = false) // 1 to N
    routerToRAMEnables.mode := modeReg
    routerToRAMEnables.src.foreach(_.setAll()) // input = all 1s

    val routerToRAMs = MontExpRouter(config, UInt(w bits), NTo1 = false) // 1 to N
    routerToRAMs.mode := modeReg
    routerToRAMs.connected := True

    when(isActive(INIT)) { // flow1 transfer the input data to productRAMs
      initCounter.increment()

      routerToRAMs.src := io.xWordIns
      routerToRAMs.selectCount := initRAMCount

      routerToRAMEnables.connected := True
      routerToRAMEnables.selectCount := initRAMCount

      addrToWrite := initWordCount
    }.elsewhen(isWritingBack) { // flow 7 & 8 transfer the MontMult result to productRAMs 7 xMontRAMs
      routerToRAMs.src := shiftedMontMultOutputs
      routerToRAMs.selectCount := outputRAMCount

      when(!writeBackLastWord) {
        routerToRAMEnables.connected := True
        routerToRAMEnables.selectCount := outputRAMCount
      }.otherwise {
        productLasts := dataToWrite
        when(writeXMont)(xMontLasts := dataToWrite)
      }
      addrToWrite := outputWordCount
    }
    dataToWrite := routerToRAMs.toDes
    ramEnables := Vec(routerToRAMEnables.toDes.map(_.asBool))
    // BLOCK flow2
    // dual-port, as when SQUARE is active, productRAMs are read concurrently in two different manners
    val productWordsForX = Mux(!lastRound, Vec(productRAMs.map(ram => ram.readAsync(xWordCount))), productLasts)
    val productBitsForX = Vec(productWordsForX.map(word => word(xBitCount).asUInt))
    when(isFeedingX)(xCounter.increment())
    when(montMultOver)(xCounter.clear())

    val routerProdToX = MontExpRouter(config, UInt(1 bits))
    routerProdToX.connected := isFeedingX
    routerProdToX.mode := modeReg
    routerProdToX.selectCount := xRAMCount
    routerProdToX.src := productBitsForX
    montMult.io.xiIns.allowOverride
    montMult.io.xiIns := routerProdToX.toDes
    // BLOCK flow3 & 4 & 5
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
        when(!lastWord)(when(firstWord)(feed1()).otherwise(feed0()))
          .otherwise(feed0())
      }

    val routerToY = MontExpRouter(config, UInt(w bits))
    routerToY.connected := isFeedingYM
    routerToY.mode := modeReg
    routerToY.selectCount := ymRAMCount
    routerToY.src := wordsForY
    montMult.io.YWordIns := routerToY.toDes

    // BLOCK flow6
    val modulusWordForM = Mux(!lastWord, modulusWordRAM.readAsync(ymCount), U(0, w bits))
    when(isFeedingYM)(montMult.io.MWordIns.foreach(_ := modulusWordForM)) // M is from the modulusRAM)

    // BLOCK flow9
    when(isActive(POST) && lastRound)(MontExpValid.set()) // set valid of the final result at the last round of POST
    when(io.valids(0).fall())(MontExpValid.clear()) // and cleared when the continuous output ends, then wait for next POST
    io.dataOuts := shiftedMontMultOutputs
    io.valids.zip(MontMultValids).foreach { case (io, mult) => io := mult && MontExpValid } // delay the valid of montMult for shifting
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

