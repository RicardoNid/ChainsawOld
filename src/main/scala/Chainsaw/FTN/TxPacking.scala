package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import breeze.numerics.ceil
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

// TODO: support different channel configuration dynamically in the same packing module
case class TxPacking(implicit ftnParams: FtnParams)
  extends Component with DSPTestable[Vec[UInt], Vec[UInt]] {

  import ftnParams._

  override val dataIn = slave Stream Vec(UInt(6 bits), 512) //
  override val dataOut = master Stream Vec(UInt(6 bits), 128)
  override val latency = 100 // meaningless

  val validLength = (channelInfo.bitMask.sum + 1) * 2 // valid length out of 512
  val validWithCP = validLength + cpLength
  val fullWithCP = fftSize + cpLength

  val packedLength = fullWithCP * 2 + validWithCP * 16
  logger.info(s"txPacked length = $packedLength")
  val stateNum = ceil(packedLength / 128.0).toInt // time period of dataOut
  require(stateNum <= 80)
  logger.info(s"period for txPacked = $stateNum")

  val memType = HardType(Vec(UInt(6 bits), validWithCP))
  val addrType = HardType(UInt(log2Up(validWithCP) bits))

  val preambleRom: Mem[Vec[UInt]] = Mem(ftnParams.preambleScaled.map(_ + 32).map(U(_, 6 bits))
    .grouped(128).toSeq.map(_.padTo(128, UInt(6 bits).getZero)) // vec size = 128
    .map(Vec(_)))

  val ping, pong: Mem[Vec[UInt]] = Mem(memType, 16)

  val readPointer: Bool = RegInit(False)
  val writePointer: Bool = RegInit(False)
  val counterIn = Counter(16)
  val counterOut = Counter(stateNum)

  // ping-pong state machine
  // TODO：make this a solid routine
  val fsm: StateMachine = new StateMachine {

    when(counterIn.willOverflow)(writePointer := ~writePointer)
    when(counterOut.willOverflow)(readPointer := ~readPointer)

    val EMPTY = StateEntryPoint()
    val HALF, FULL = State()

    EMPTY.whenIsActive(when(counterIn.willOverflow)(goto(HALF)))
    FULL.whenIsActive(when(counterOut.willOverflow)(goto(HALF)))
    HALF.whenIsActive {
      when(counterIn.willOverflow && counterOut.willOverflow)(goto(HALF))
        .elsewhen(counterIn.willOverflow)(goto(FULL))
        .elsewhen(counterOut.willOverflow)(goto(EMPTY))
    }

    dataIn.ready := isActive(EMPTY) || isActive(HALF)
    when(dataIn.fire)(counterIn.increment())
    
    when(isActive(FULL) || isActive(HALF))(counterOut.increment())
    dataOut.valid := Delay(isActive(FULL) || isActive(HALF), 4, init = False)
  }

  // writing logic
  val dataInWithCP = {
    val validPart = dataIn.payload.take(validLength)
    Vec(validPart.takeRight(10) ++ validPart ++ validPart.take(10))
  }
  ping.write(counterIn.value, dataInWithCP, dataIn.fire && writePointer)
  pong.write(counterIn.value, dataInWithCP, dataIn.fire && ~writePointer)

  // reading logic
  // addrs corresponding to different states
  val timeForPreamble = ceil(fullWithCP * 2 / 128.0).toInt
  val offset = 128 - (fullWithCP * 2) % 128 // first part of data

  val addrsOfPreambleVec: Seq[Int] = (0 until timeForPreamble).padTo(stateNum, 0)
  val addrsOfData = (0 until stateNum - timeForPreamble).map(i => i * 128 + offset)
  val addrsOfDataVec = Seq.fill(timeForPreamble)(0) ++ // while reading preamble
    addrsOfData.map(_ / validWithCP)
  val addrsInDataVec = Seq.fill(timeForPreamble)(0) ++ // while reading preamble
    addrsOfData.map(_ % validWithCP)

  assert(addrsOfPreambleVec.length == stateNum && addrsOfDataVec.length == stateNum && addrsInDataVec.length == stateNum)

  // output logic
  val romAddrOfPreambleVec = Mem(addrsOfPreambleVec.map(U(_, log2Up(addrsOfPreambleVec.max + 1) bits)))
  val romAddrOfDataVec: Mem[UInt] = Mem(addrsOfDataVec.map(U(_, log2Up(addrsOfDataVec.max + 1) bits)))
  val romAddrInDataVec: Mem[UInt] = Mem(addrsInDataVec.map(U(_, log2Up(addrsInDataVec.max + 1) bits)))

  // level0 -> level1
  val addrOfPreambleVec = romAddrOfPreambleVec.readSync(counterOut.value)
  val addrOfDataVec = romAddrOfDataVec.readSync(counterOut.value)
  val addrInDataVec = romAddrInDataVec.readSync(counterOut.value)

  // level1 -> level2
  val preambleRead = preambleRom.readSync(addrOfPreambleVec)
  val dataNow = Mux(Delay(readPointer, 2), ping.readSync(addrOfDataVec), pong.readSync(addrOfDataVec))
  val dataNext = Mux(Delay(readPointer, 2), ping.readSync(addrOfDataVec + U(1)), pong.readSync(addrOfDataVec + U(1)))

  // level2 -> level3
  val preambleDelayed1 = RegNext(preambleRead)
  val dataCombined = Vec(RegNext(dataNow) ++ RegNext(Vec(dataNext.take(128)))) // as the output may cross the boundary of two vecs

  // level3 -> level4
  val preambleDelayed2 = RegNext(preambleDelayed1)
  // fixme: bad critical path in big shifter
  val dataExtracted = RegNext(Vec(dataCombined.rotateLeft(Delay(addrInDataVec, 2)).take(128)))

  val lastLength = (validWithCP * 16 - offset) % 128

  val innerPayload = cloneOf(dataOut.payload)
  switch(Delay(counterOut.value, 4)) {
    (0 until 8).foreach(i => is(U(i))(innerPayload := preambleDelayed2))
    is(U(8))(innerPayload := Vec(preambleDelayed2.take(128 - offset) ++ dataExtracted.take(offset)))
    is(U(stateNum - 1))(innerPayload := Vec(dataExtracted.take(lastLength).padTo(128, UInt(6 bits).getZero))) // last period, padding
    default(innerPayload := dataExtracted)
  }

  dataOut.payload := Mux(dataOut.valid, innerPayload, innerPayload.getZero)
}

object TxPacking extends App {
  implicit val ftnParams = FtnParams(3,226,true)
  VivadoSynthForTiming(TxPacking())
}

