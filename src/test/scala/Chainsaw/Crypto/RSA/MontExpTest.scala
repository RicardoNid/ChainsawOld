package Chainsaw.Crypto.RSA

import Chainsaw.{DSPRand, GenRTL, _}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimConfig, simTime, _}

import scala.collection.mutable.ArrayBuffer

class MontExpTest extends AnyFunSuite {
  test("testMontExp") {

    val lN = 512

    // print the padded number in hex form
    def printPadded(name: String, value: BigInt, n: Int): Unit = {
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

        val prodRegsLowForWatch = doubleLengthDataOut(lN - 1 downto 0)
        prodRegsLowForWatch.simPublic()

        import fsm._

        val isINIT = isActive(INIT)
        val isPRECOM = isActive(PRECOM)
        val isPRE = isActive(PRE)
        val isRUNNING = isActive(DoMultFor1) || isActive(DoSquareFor0) || isActive(DoSquareFor1)
        val isPOST = isActive(POST)
        val isBOOT = isActive(stateBoot)

        isINIT.simPublic()
        isPRECOM.simPublic()
        isPRE.simPublic()
        isRUNNING.simPublic()
        isPOST.simPublic()
        isBOOT.simPublic()
        innerCounter.value.simPublic()
        pipelineCounter.value.simPublic()
        reductionRet.simPublic()
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
        val rhoSquare = algo.getRhoSquare(N)
        val omega = algo.getOmega(N)

        val inputValues = (0 until pipelineFactor).map(_ => BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000))
        val aMonts = inputValues.map(algo.montMul(_, rhoSquare, N))
        val results = inputValues.map(algo.montExp(_, exponent, N))
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
        val yourRecord = ArrayBuffer[BigInt]()
        var dutResult = BigInt(0)
        // simulation and monitoring

        val cyclesForExponent = exponent.toString(2).tail.map(_.asDigit + 1).sum * 3

        ChainsawDebug = false
        (0 until (cyclesForExponent + lN) * mult.latency + 50).foreach { _ =>

          def count = innerCounter.value.toInt

          def pipelineCount = pipelineCounter.value.toInt

          dut.clockDomain.waitSampling()

          // record the intermediate
          if(dut.isPOST.toBoolean){
            if(count == 3 && pipelineCount == 2) dutResult = dut.reductionRet.toBigInt
          }
        }

        // output
        if (ChainsawDebug) {
          // print something
        }
        else assertResult(results(pipelineFactor - 1))(dutResult) // result assertion

        println(s"cycles for exponent should be ${exponent.toString(2).tail.map(_.asDigit + 1).sum * 3 * mult.latency}")
      }
  }
}