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
    val ref = new RSARef(lN)
    val algo = new RSAAlgo(lN)

    val N = BigInt(ref.getModulus)

    val exponent = ref.getPrivateValue
    val exponentLength = ref.getPrivateValue.bitLength()
    // pad to right as the hardware design requires
    val paddedExponent = BigInt(exponent.toString(2).padTo(lN, '0'), 2)

    ChainsawDebug = true
    val rhoSquare = algo.getRhoSquare(N)
    val omega = algo.getOmega(N)

    ChainsawDebug = false
    val inputValue = BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000)
    val aMont = algo.montMul(inputValue, rhoSquare, N)
    val result = algo.montExp(inputValue, exponent, N)
    val record = algo.montExpWithRecord(inputValue, exponent, N)

    val toPrint = Map(
      "N" -> (N, lN),
      "omega" -> (omega, lN), "rhoSquare" -> (rhoSquare, lN),
      "aMont" -> (aMont, lN), "result" -> (result, lN)
    )
    toPrint.foreach { case (str, tuple) => printPadded(str, tuple._1, tuple._2) }

    //    ChainsawDebug = true
    SimConfig.withWave.compile(
      new MontExp(lN) {

        val stateCountDown = fsm.PRECOM.cache.value
        stateCountDown.simPublic()

        val prodRegsLowForWatch = doubleLengthReg(lN - 1 downto 0)
        prodRegsLowForWatch.simPublic()

        fsm.isPRE.simPublic()
        fsm.isPOST.simPublic()
        fsm.isINIT.simPublic()
        fsm.isPRECOM.simPublic()
        fsm.isBOOT.simPublic()
        innerCounter.value.simPublic()
        reductionRet.simPublic()
      })
      .doSim { dut =>
        import dut._
        dut.clockDomain.forkStimulus(2)

        // poking to input
        dut.input.N #= N
        dut.input.exponent #= paddedExponent
        dut.input.exponentLength #= exponentLength

        dut.input.value #= inputValue

        val yourRecord = ArrayBuffer[BigInt]()

        val start = ArrayBuffer[Long]()
        val end = ArrayBuffer[Long]()
        var dutResult = BigInt(0)
        // simulation and monitoring

        val cyclesForExponent = exponent.toString(2).tail.map(_.asDigit + 1).sum * 3

        (0 until (cyclesForExponent + lN) * mult.latency + 50).foreach { _ =>

          def count = innerCounter.value.toInt

          dut.clockDomain.waitSampling()
          if (dut.fsm.isPRE.toBoolean && count == 2) start += (simTime + 2)
          if (dut.fsm.isPOST.toBoolean && count == 0) end += simTime
          if (dut.innerCounter.value.toInt == 0
            && !dut.fsm.isINIT.toBoolean
            && !dut.fsm.isBOOT.toBoolean
            && !fsm.isPRECOM.toBoolean)
            yourRecord += dut.reductionRet.toBigInt
          if (fsm.isPOST.toBoolean && count == 3)
            dutResult = reductionRet.toBigInt
        }

        if (ChainsawDebug) {
          println(record.size)
          println(yourRecord.size)
          yourRecord.zip(record).foreach { case (int, int1) =>
            printPadded("yours ", int, lN)
            printPadded("golden", int1, lN)
          }
          printPadded("your result   ", dutResult, lN)
          printPadded("golden result ", result, lN)
        }
        else assertResult(result)(dutResult) // result assertion

        println(s"cycles for exponent should be ${exponent.toString(2).tail.map(_.asDigit + 1).sum * 3 * mult.latency}")
        println(s"cycles actually comsumed: ${(end(0) - start(0)) / 2}")
      }
  }
}