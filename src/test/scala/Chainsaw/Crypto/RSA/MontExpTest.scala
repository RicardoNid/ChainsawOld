package Chainsaw.Crypto.RSA

import Chainsaw.{DSPRand, GenRTL, _}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimConfig, simTime, _}

import scala.collection.mutable.ArrayBuffer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class MontExpTest extends AnyFunSuite {
  test("testMontExp") {

    val lN = 512

    // print the padded number in hex form
    def printPadded(name: String, value: BigInt, n: Int = lN): Unit = {
      val hex =
        value.toString(2).padToLeft(n, '0')
          .grouped(4).toArray.map(BigInt(_, 2).toString(16))
          .mkString("")
      println(s"$name = $hex")
    }

    GenRTL(new MontExp(lN))

    //    ChainsawDebug = true
    SimConfig.withWave.compile(
      new MontExp(lN) {
        val stateCountDown = fsm.PRECOM.cache.value
        stateCountDown.simPublic()

        val prodRegsLowForWatch = doubleLengthQueue.io.pop.payload(lN - 1 downto 0)
        //        val prodRegsLowForWatch = doubleLengthDataOut(lN - 1 downto 0)
        prodRegsLowForWatch.simPublic()

        import fsm._

        val isINIT = isActive(INIT)
        val isPRECOM = isActive(PRECOM)
        val isPRE = isActive(PRE)
        val isRUNNING = isActive(DoMultFor1) || isActive(DoSquareFor0) || isActive(DoSquareFor1)
        val isPOST = isActive(POST)
        val isBOOT = isActive(stateBoot)

        val subInput0Low = addSub.input(0)(lN downto 0)
        val rPrime = subInput0Low >> 1
        val addInput0Low = modRho(addSub.input(0))
        val addInput1Low = modRho(addSub.input(1))
        val addOutputLow = modRho(addSub.output)
        val subOutputLow = addSub.output(lN downto 0)
        subInput0Low.simPublic()
        rPrime.simPublic()
        subOutputLow.simPublic()
        addInput0Low.simPublic()
        addInput1Low.simPublic()
        addOutputLow.simPublic()

        isINIT.simPublic()
        isPRECOM.simPublic()
        isPRE.simPublic()
        isRUNNING.simPublic()
        isPOST.simPublic()
        isBOOT.simPublic()
        operationCounter.value.simPublic()
        pipelineCounter.value.simPublic()
        reductionRet.simPublic()
        mult.input.simPublic()
        mult.output.simPublic()
        prodLow.simPublic()
        add.input.simPublic()
        add.output.simPublic()

        getOmegaDatapath.flag.simPublic()
        getOmegaDatapath.flagAfterMul.simPublic()
        getRhoSquareDatapath.flag.simPublic()
        getRhoSquareDatapath.flagAfterAdd.simPublic()

        omegaRegs.simPublic()
        rhoSquareReg.simPublic()

        val omegaReverse = mult.input(0).asBools.grouped(4).toSeq.reverse.flatten.asBits().asUInt
        omegaReverse.simPublic()
      })
      .doSim { dut =>

        import dut._

        // preparing data
        val ref = new RSARef(lN)
        val algo = new RSAAlgo(lN)

        val N = BigInt(ref.getModulus)
        val exponent = BigInt(ref.getPublicValue)
        val exponentLength = ref.getPublicValue.bitLength()
        // pad to right as the hardware design requires
        val paddedExponent = BigInt(exponent.toString(2).padTo(lN, '0'), 2)

        ChainsawDebug = false
        val rhoSquare = algo.getRhoSquare(N, false)
        val omega = algo.getOmega(N, print = false)

        val inputValues = (0 until pipelineFactor).map(_ => BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000))
        val aMonts = inputValues.map(algo.montMul(_, rhoSquare, N))
        val results = inputValues.map(algo.montExp(_, exponent, N))
        algo.montExp(inputValues.head, exponent, N, print = true)
        val records = inputValues.map(algo.montExpWithRecord(_, exponent, N))

        val toPrint = Map("N" -> (N, lN), "exponent" -> (exponent, lN), "omega" -> (omega, lN), "rhoSquare" -> (rhoSquare, lN))
        toPrint.foreach { case (str, tuple) => printPadded(str, tuple._1, tuple._2) }
        println(s"exponentLength = $exponentLength")
        inputValues.foreach(printPadded("input", _, lN))
        aMonts.foreach(printPadded("aMont", _, lN))
        results.foreach(printPadded("result", _, lN))

        dut.clockDomain.forkStimulus(2)

        // poking parameters
        dut.input.N #= N
        dut.input.exponent #= paddedExponent
        dut.input.exponentLength #= exponentLength

        // poking inputs
        (0 until pipelineFactor).foreach { i =>
          clockDomain.waitSampling()
          dut.input.value #= inputValues(i)
        }

        // data to be recorded
        val dutResult = ArrayBuffer[BigInt]()
        // simulation and monitoring

        val cyclesForExponent = exponent.toString(2).tail.map(_.asDigit + 1).sum * 3


        def count = operationCounter.value.toInt

        def pipelineCount = pipelineCounter.value.toInt

        ChainsawDebug = false
        (0 until (cyclesForExponent + lN) * pipelineFactor + precomCycles).foreach { _ =>

          dut.clockDomain.waitSampling()

          // record the intermediate
          if (valid.toBoolean) dutResult += dut.reductionRet.toBigInt
          if (isPRE.toBoolean && operationCycle.toInt == 0 && pipelineCycle.toInt == 0) {
            assertResult(omegaRegs.toBigInt)(omega)
            assertResult(rhoSquareReg.toBigInt)(rhoSquare)
            printlnGreen("Assertion Done")
          }
          //          if (dut.isPRE.toBoolean || dut.isRUNNING.toBoolean) {
          //            if (pipelineCount == 0) {
          //              if (count == 0) {
          //                printPadded("a          ", mult.input(0).toBigInt)
          //                printPadded("b          ", mult.input(1).toBigInt)
          //                printPadded("previous UN", mult.output.toBigInt, 2 * lN)
          //                printPadded("previous t ", add.input(0).toBigInt, 2 * lN)
          //                printPadded("previous UN", add.input(1).toBigInt, 2 * lN)
          //                printPadded("prev t + UN", add.output.toBigInt, 2 * lN)
          //              } else if (count == 1) {
          //                printPadded("t          ", mult.input(0).toBigInt)
          //                printPadded("omega      ", mult.input(1).toBigInt)
          //                printPadded("ab         ", mult.output.toBigInt, 2 * lN)
          //              } else if (count == 2) {
          //                printPadded("U          ", mult.input(0).toBigInt)
          //                printPadded("N          ", mult.input(1).toBigInt)
          //                printPadded("omega * t  ", mult.output.toBigInt, 2 * lN)
          //              }
          //            }
          //          }
        }

        dutResult.foreach(printPadded("dutResult", _, lN))
        //        dutResult.zip(results).foreach { case (int, int1) => assertResult(int)(int1) }
      }
  }
}