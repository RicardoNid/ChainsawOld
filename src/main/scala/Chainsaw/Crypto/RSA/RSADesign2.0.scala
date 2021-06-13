//package Chainsaw.Crypto.RSA
//
//import Chainsaw._
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//
//case class MontExpInput(lN: Int) extends Bundle {
//  val N = UInt(lN bits)
//  val exponent = UInt(lN bits)
//  val exponentLength = UInt(log2Up(lN) + 1 bits) // the exponent can be lN-bit long
//  val value = UInt(lN bits)
//}
//
//// first version, design with single, big multiplier
//class MontExp(lN: Int, mulLatency: Int = 5, addLatency: Int = 2) extends DSPDUTTiming[MontExpInput, UInt] {
//  override val input: MontExpInput = in(MontExpInput(lN))
//  override val output: UInt = out(UInt(lN bits))
//  output := U(0)
//  val valid = out(RegInit(False))
//  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)
//
//  // design parameters
//  val pipelineDepth = mulLatency + 2 * addLatency
//  val precomOperations = lN + 2
//  val precomCycles = precomOperations * pipelineDepth
//
//  // operator modules
//  val mult = new BigMult(lN, mulLatency)
//  val add = new BigAdd(2 * lN, addLatency)
//  val sub = new BigSub(lN + 1, addLatency) // as the max length is lN + 1 when you go through getRhoSquare datapath
//  Seq(mult, add, sub).foreach(_.input.foreach(_.clearAll())) // preassign to avoid latches
//
//  // regs for data
//  val inputValueRegs = Vec(Reg(UInt(lN bits)), pipelineDepth) // would be reused for aMont
//  val exponentReg = Reg(UInt(lN bits))
//  val exponentLengthReg = Reg(UInt(log2Up(lN) + 1 bits))
//  val NReg = Reg(UInt(lN bits))
//  // general fifos for intermediate data, you can easily adjust your queues here
//
//  val QueueWidths = Seq(1, lN + 1, lN + 1, 2 * lN, 2 * lN)
//  val QueueDepths = Seq.fill(5)(pipelineDepth + 1)
//
//  // as they have different width and shared depth
//  val fifos = QueueWidths.zip(QueueDepths).map { case (i, i1) => FIFO(UInt(i bits), i1) }
//  val Seq(bitQueue, singleLengthQueue, anotherSingleLengthQueue, doubleLengthQueue, anotherDoubleLengthQueue) = fifos // use unapply to declare fifos in batch
//  fifos.foreach (_.io.pop.ready.allowOverride)
//  val omegaRegs = Reg(UInt(lN bits))
//  val rhoSquareReg = Reg(UInt(lN bits))
//  // TODO: improve this
//  val modMask = RegInit(B(BigInt(3), lN bits)) // mask for mod in getOmega progress
//  val addMask = RegInit(B(BigInt(2), lN bits)) // mask for add in getOmega progress
//  // counters, they are the controller of the schedule
//  val pipelineCounter = Counter(pipelineDepth)
//  val operationCounter = Counter(precomCycles, inc = pipelineCounter.willOverflow) // counter used inside every StateDelay for control
//  val operationCounterHistory = History(operationCounter.value, mulLatency + addLatency + 1)
//  val Seq(oCAfterMul, oCAfterAdd, oCAfterMulAdd) = // opertionCounter after k, l, k+l cycles
//    Seq(mulLatency, addLatency, mulLatency + addLatency).map(operationCounterHistory(_))
//  val exponentCounter = Counter(lN) // counter
//  // flags
//  val currentExponentBit = exponentReg(lN - 2) // the second most importatnt exponent bit is the decisive one
//  val exponentEnd = exponentCounter.valueNext === (exponentLengthReg - 1)
//  val saveAMont = RegInit(False) // flag
//
//  // utilities and subroutines
//  def doubleLengthLow = lowerlN(doubleLengthQueue.pop())
//  // mult.output acts like a register as mult is end-registered
//  def prodLow = mult.output(lN - 1 downto 0)
//  // << 1 leads to on bit more, resize would take lower(right) bits
//  def lowerlN[T <: BitVector](value: T) = value(lN - 1 downto 0)
//  def divideRho(value: UInt) = value >> lN
//  //  def exponentMove() = inputRegs.exponent := (inputRegs.exponent << 1).resized
//  def exponentMove() = exponentReg := (exponentReg << 1).resized
//  def maskMove() = {
//    modMask := modMask(lN - 2 downto 0) ## True
//    addMask := (addMask << 1).resized
//  }
//  def pipelineCycle = pipelineCounter.value
//  def operationCycle = operationCounter.value
//
//  def atOperation(count: Int) = operationCounter.value === U(count)
//  def atOperationAfterMul(count: Int) = oCAfterMul === U(count)
//  def atOperationAfterAdd(count: Int) = oCAfterAdd === U(count)
//  def atOperationAfterMulAdd(count: Int) = oCAfterMulAdd === U(count)
//
//  def atPipelineCycle(count: Int) = pipelineCycle === U(count)
//
//  val montMulDatapath = new Area {
//
//    val input0, input1, ret = UInt(lN bits)
//    Seq(input0, input1, ret).foreach(_.clearAll())
//
//    val flag = Bool()
//    flag.clear()
//    val flagAfterMul = Delay(flag, mulLatency, init = False)
//    val flagAfterMulAdd = Delay(flag, mulLatency + addLatency, init = False)
//
//    //    when(flag) { // at X_0
//    //      when(atOperation(0)) { // 0_0
//    //        mult.doMult(input0, input1) // a * b
//    //        val det = sub.output(lN downto 0)
//    //        ret := Mux(det.msb, lowerlN(singleLengthQueue.pop()), lowerlN(det))
//    //      }
//    //        .elsewhen(atOperation(1)) { // 1_0
//    //          mult.doMult(lowerlN(doubleLengthQueue.pop()), omegaRegs) // t mod rho * omega
//    //          anotherDoubleLengthQueue.push(doubleLengthQueue.pop()) // t will be used again later
//    //        }
//    //        .elsewhen(atOperation(2))(mult.doMult(lowerlN(singleLengthQueue.pop()), NReg)) // 2_0 U * N
//    //    }
//
//    when(flag) { // at X_0
//      switch(operationCycle) {
//        is(U(0)) { // starts from 0_0
//          mult.doMult(input0, input1) // a * b
//          val det = sub.output(lN downto 0)
//          ret := Mux(det.msb, lowerlN(singleLengthQueue.pop()), lowerlN(det)) // ret of previous MontMul
//        }
//        is(U(1)) { // starts from 1_0
//          mult.doMult(lowerlN(doubleLengthQueue.pop()), omegaRegs) // t mod rho * omega
//          anotherDoubleLengthQueue.push(doubleLengthQueue.pop()) // t will be used again later
//        }
//        is(U(2)) { // starts from 2_0
//          mult.doMult(lowerlN(singleLengthQueue.pop()), NReg) // U * N
//        }
//      }
//    }
//
//    when(flagAfterMul) { // at X_k
//      when(oCAfterMul === U(0))(doubleLengthQueue.push(mult.output)) // 0_k t = a * b in
//      when(oCAfterMul === U(1))(singleLengthQueue.push(lowerlN(mult.output))) // 1_k U = rho * omega mod rho in
//      when(oCAfterMul === U(2))(add.doAdd(mult.output, anotherDoubleLengthQueue.pop())) // 2_k t out UN + t
//    }
//    when(flagAfterMulAdd) { // at X_k+l
//      when(oCAfterMulAdd === U(2)) { // 2_k+l
//        val mid = add.output(2 * lN downto lN) // mid = (UN + t) / rho
//        sub.doSub(mid.resize(lN + 1), NReg) // det = mid - N
//        singleLengthQueue.push(mid)
//      }
//    }
//  }
//
//  def setMontMulDatapath(input0: UInt, input1: UInt) = {
//    montMulDatapath.flag := True
//    montMulDatapath.input0 := input0
//    montMulDatapath.input1 := input1
//  }
//
//  val getOmegaDatapath = new Area {
//    val flag = Bool() // flag
//    flag.clear()
//    val flagAfterMul = Delay(flag, mulLatency, init = False) // not init would lead to leakage
//    val flagAfterAdd = Delay(flag, addLatency, init = False)
//    when(flag) {
//      def newSolution() = Mux(bitQueue.pop().asBool,
//        lowerlN(singleLengthQueue.pop()), // solution
//        lowerlN(singleLengthQueue.pop()) | addMask.asUInt) // solution + (1 << (exp + 1))
//
//      when(atOperation(0)) { // starts from f(1) = 0 mod 2
//        mult.doMult(U(1), NReg) // initial solution = 1, 1 * N
//        singleLengthQueue.push(U(1).resized) // save the solution
//      }.elsewhen(atOperation(precomOperations - 2)) { // lN_0, final solution resolved, wait for the adder tobe available
//        add.doAdd(~singleLengthQueue.pop(), U(1))
//      }.elsewhen(atOperation(precomOperations - 1)) { // lN+1_0
//        // do nothing
//      }.otherwise { // 1_0 ~ lN-1_0
//        mult.doMult(newSolution(), NReg) // iteratively go for next solution
//        singleLengthQueue.push(newSolution()) // save the solution
//        when(atPipelineCycle(pipelineDepth - 1))(maskMove()) // FIXME: separate two mask
//      }
//    }
//    when(flagAfterMul) { // 0_k ~ lN-1_k
//      val det = (prodLow & modMask.asUInt) === U(1)
//      bitQueue.push(det.asUInt)
//    }
//    when(flagAfterAdd) {
//      //      when(operationCounterAfterAdd === U(lN + 1))(omegaRegs := lowerlN(addSub.output)) // lN+2_l
//      when(oCAfterAdd === U(precomOperations - 2))(omegaRegs := lowerlN(add.output)) // lN+2_l, store the result
//    }
//  }
//
//  val getRhoSquareDatapath = new Area {
//    val flag = Bool()
//    flag.clear()
//    val flagAfterAdd = Delay(flag, addLatency, init = False)
//    def getRPrime() = {
//      val det = anotherSingleLengthQueue.pop() // fetch saved determinant
//      val r = lowerlN(doubleLengthQueue.pop()) // fetch saved solution
//      Mux(det.msb, lowerlN(r << 1), lowerlN(det)) // r' = 2 * r - N or 2 * r
//    }
//    when(flag) {
//      when(atOperation(0)) {
//        sub.doSub(U(BigInt(1) << lN), NReg) // 2 * r - N
//        doubleLengthQueue.push(U(BigInt(1) << (lN - 1))) // r in
//      }.elsewhen(atOperation(precomOperations - 1)) {
//        rhoSquareReg := getRPrime() // TODO: can be done as early as at lN_l, rather than lN+1_0
//      }.otherwise {
//        val rPrime = getRPrime()
//        sub.doSub(rPrime << 1, NReg) // 2 * r' - N
//        doubleLengthQueue.push(rPrime.resized) // r' in, save the solution
//      }
//    }
//    when(flagAfterAdd) {
//      val det = sub.output(lN downto 0)
//      anotherSingleLengthQueue.push(det)
//    }
//  }
//
//  val fsm = new StateMachine {
//    // state declarations
//    val INIT = new StateDelayFixed(pipelineDepth) with EntryPoint
//    val PRECOM = new StateDelayFixed(precomCycles) // precomputation of omega and RhoSquare
//    // states that executes the MontMul
//    val PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST = new StateDelayFixed(3 * pipelineDepth)
//
//    // the overall control strategy: by the innerCounter
//    val allStateDelays = Seq(INIT, PRECOM, PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST)
//    allStateDelays.foreach(_.whenIsActive {
//      pipelineCounter.increment()
//    })
//    allStateDelays.foreach(_.whenCompleted(operationCounter.clear()))
//
//    val stateTransition = new Area { // state transitions and counter maintenances
//      INIT.whenCompleted(goto(PRECOM))
//      PRECOM.whenCompleted(goto(PRE))
//      PRE.whenCompleted {
//        exponentMove()
//        exponentCounter.clear()
//        when(currentExponentBit)(goto(DoSquareFor1))
//          .otherwise(goto(DoSquareFor0))
//      }
//      DoSquareFor1.whenCompleted(goto(DoMultFor1))
//      DoMultFor1.whenCompleted(goto(DoMultFor1)).whenCompleted {
//        exponentMove()
//        exponentCounter.increment()
//        when(exponentEnd)(goto(POST))
//          .elsewhen(currentExponentBit)(goto(DoSquareFor1))
//          .otherwise(goto(DoSquareFor0))
//      }
//      DoSquareFor0.whenCompleted {
//        exponentMove()
//        exponentCounter.increment()
//        when(exponentEnd)(goto(POST))
//          .elsewhen(currentExponentBit)(goto(DoSquareFor1))
//          .otherwise(goto(DoSquareFor0))
//      }
//      POST.whenCompleted(goto(INIT))
//    }
//
//    // TODO: merge INIT workload with PRECOM ?
//    val workload = new Area { // state workload on datapath
//      INIT.whenIsActive {
//        NReg := input.N
//        exponentReg := input.exponent
//        exponentLengthReg := input.exponentLength
//        inputValueRegs(pipelineCycle) := input.value
//      } // data initialization
//      PRECOM.whenIsActive { // computing omega and rhoSquare
//        when(operationCounter.value <= lN + 1)(getRhoSquareDatapath.flag := True)
//        getOmegaDatapath.flag := True
//      }
//      PRE.whenIsActive(setMontMulDatapath(inputValueRegs(pipelineCycle), rhoSquareReg))
//      PRE.whenCompleted(saveAMont.set()) // extra work
//      when(saveAMont) {
//        inputValueRegs(pipelineCycle) := montMulDatapath.ret
//        when(atPipelineCycle(pipelineDepth - 1))(saveAMont := False)
//      }
//      DoSquareFor1.whenIsActive(setMontMulDatapath(montMulDatapath.ret, montMulDatapath.ret))
//      DoMultFor1.whenIsActive(setMontMulDatapath(montMulDatapath.ret, inputValueRegs(pipelineCycle)))
//      DoSquareFor0.whenIsActive(setMontMulDatapath(montMulDatapath.ret, montMulDatapath.ret))
//      POST.whenIsActive(setMontMulDatapath(montMulDatapath.ret, U(1)))
//      POST.whenCompleted(valid.set())
//      when(valid) {
//        val det = sub.output(lN downto 0)
//        output := Mux(det.msb, lowerlN(singleLengthQueue.pop()), lowerlN(det))
//        when(atPipelineCycle(pipelineDepth - 1))(valid.clear())
//      }
//    }
//  }
//}
//
//object MontExp {
//  def main(args: Array[String]): Unit = {
//    VivadoSynth(new MontExp(512))
//    //    VivadoElabo(new MontExp(512))
//  }
//}