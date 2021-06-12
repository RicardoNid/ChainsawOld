package Chainsaw.Crypto.RSA

import Chainsaw.{DSPRand, GenRTL, _}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimConfig, _}
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

class MontExpTest extends AnyFunSuite {
  test("testMontExp") {

    val lN = 512
    val times = 1

    GenRTL(new MontExp(lN)) // quickly check the design semantic

    //    ChainsawDebug = true

    SimConfig.withWave.compile( // add and expose signals for debugging
      new MontExp(lN) {

        import fsm._

        // state flags
        val stateFlags = allStateDelays.map(isActive(_))
        val Seq(isINIT, isPRECOM, isPRE, isDM1, isDM0, isDS1, isPOST) = stateFlags
        val isRUNNING = isDM0 || isDM1 || isDS1
        val isBOOT = isActive(stateBoot)
        stateFlags.foreach(_.simPublic())
        isRUNNING.simPublic()
        isBOOT.simPublic()
        // counters
        operationCounter.value.simPublic()
        pipelineCounter.value.simPublic()
        // data iteration
        // stepwise result of the three datapaths
        val rPrime = sub.input(0) >> 1
        // as omega is very small(long leading 0s) at the beginning, showing its reverse would make it easier for debugging
        val omegaReverse = mult.input(0).asBools.grouped(4).toSeq.reverse.flatten.asBits().asUInt
        montMulDatapath.ret.simPublic()
        // as add is 2 * lN long and we only care about the lower part sometimes
        val addInput0Low = lowerlN(add.input(0))
        val addInput1Low = lowerlN(add.input(1))
        val addOutputLow = lowerlN(add.output)
        Seq(rPrime, omegaReverse, addInput0Low, addInput1Low, addOutputLow).foreach(_.simPublic())

        omegaRegs.simPublic()
        rhoSquareReg.simPublic()
      })
      .doSim { dut => // starts simulation
        import dut._ // for simplicity
        val algo = new RSAAlgo(lN)
        val ref = new RSARef(lN)

        def count = operationCounter.value.toInt
        def pipelineCount = pipelineCounter.value.toInt
        def printMontMulData() = {
          if (dut.isPRE.toBoolean || dut.isRUNNING.toBoolean) {
            if (pipelineCount == 0) {
              if (count == 0) {
                printPadded("a          ", mult.input(0).toBigInt, lN)
                printPadded("b          ", mult.input(1).toBigInt, lN)
                printPadded("previous UN", mult.output.toBigInt, 2 * lN)
                printPadded("previous t ", add.input(0).toBigInt, 2 * lN)
                printPadded("previous UN", add.input(1).toBigInt, 2 * lN)
                printPadded("prev t + UN", add.output.toBigInt, 2 * lN)
              } else if (count == 1) {
                printPadded("t          ", mult.input(0).toBigInt, lN)
                printPadded("omega      ", mult.input(1).toBigInt, lN)
                printPadded("ab         ", mult.output.toBigInt, 2 * lN)
              } else if (count == 2) {
                printPadded("U          ", mult.input(0).toBigInt, lN)
                printPadded("N          ", mult.input(1).toBigInt, lN)
                printPadded("omega * t  ", mult.output.toBigInt, 2 * lN)
              }
            }
          }
        }

        ChainsawDebug = true

        def run() = {
          // preparing input data
          val N = BigInt(ref.getModulus)
          val exponent = BigInt(ref.getPublicValue)
          val exponentLength = ref.getPublicValue.bitLength()
          // pad to right as the hardware design requires
          val paddedExponent = BigInt(exponent.toString(2).padTo(lN, '0'), 2)
          val inputValues = (0 until pipelineDepth) // randomized input
            .map(_ => BigInt(ref.getPrivateValue) / DSPRand.nextInt(1000) - DSPRand.nextInt(10000))
          // preparing result data
          val rhoSquare = algo.getRhoSquare(N, false)
          val omega = algo.getOmega(N, print = false)
          val aMonts = inputValues.map(algo.montMul(_, rhoSquare, N))
          val results = inputValues.map(algo.montExp(_, exponent, N))

          if (ChainsawDebug) {
            algo.montExp(inputValues.head, exponent, N, print = true) // show the lifecycle of the first input
            val toPrint = Map("N" -> (N, lN), "exponent" -> (exponent, lN), "omega" -> (omega, lN), "rhoSquare" -> (rhoSquare, lN))
            toPrint.foreach { case (str, tuple) => printPadded(str, tuple._1, tuple._2) } // show the parameters(single)
            println(s"exponentLength = $exponentLength")
            inputValues.foreach(printPadded("input", _, lN)) // show the results(multiple)
            aMonts.foreach(printPadded("aMont", _, lN))
            results.foreach(printPadded("result", _, lN))
          }
          // data to be recorded
          val dutResult = ArrayBuffer[BigInt]()
          // simulation and monitoring
          val cyclesForExponent = exponent.toString(2).tail.map(_.asDigit + 1).sum * 3

          dut.clockDomain.forkStimulus(2)

          // poking parameters
          dut.input.N #= N
          dut.input.exponent #= paddedExponent
          dut.input.exponentLength #= exponentLength

          // poking inputs
          (0 until pipelineDepth).foreach { i =>
            clockDomain.waitSampling()
            dut.input.value #= inputValues(i)
          }
          (0 until (cyclesForExponent + lN) * pipelineDepth + precomCycles).foreach { _ =>
            dut.clockDomain.waitSampling()
            // record the intermediate
            if (valid.toBoolean) dutResult += output.toBigInt
            if (isPRE.toBoolean && operationCycle.toInt == 0 && pipelineCycle.toInt == 0) {
              assertResult(omega)(omegaRegs.toBigInt)
              assertResult(rhoSquare)(rhoSquareReg.toBigInt)
              printlnGreen("precom assertion Done")
            }
          }

          dutResult.foreach(printPadded("dutResult", _, lN))
          assert(dutResult.size != 0 && results.size != 0)
          dutResult.zip(results).foreach { case (int, int1) => assertResult(int1)(int) }
          printlnGreen("result assertion Done")
        }
        (0 until times).foreach(_ => run())
      }
  }
}