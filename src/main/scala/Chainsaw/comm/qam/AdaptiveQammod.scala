package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.algos.Qam.{getRms, getSymbols}
import breeze.numerics.abs
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

case class AdaptiveQammod(bitCandidates: Seq[Int], hardType: HardType[SFix]) extends Component {

  val bitWidth = bitCandidates.max

  val orderIn = in UInt (log2Up(bitCandidates.max) bits) // modulation order
  val bitsIn = in Bits (bitWidth bits)
  val symbolOut = out(ComplexNumber(hardType))

  // building rom
  val orders = (bitCandidates.min to bitWidth).map(1 << _)
  val qamValues = orders.reverse.flatMap(order => getSymbols(order).toArray.map(_ / getRms(order))) // get normalized symbols
  val padding = Seq.fill(1 << bitCandidates.min)(BComplex(0.0, 0.0))
  val qamRom = Mem((qamValues ++ padding).map(CN(_, hardType)))

  // generate addr according to the current modulation order
  val addr = qamRom.addressType()
  addr.simPublic()
  switch(orderIn) {
    bitCandidates.foreach { i =>
      is(U(i - 1))(addr := RegNext(B("1" * (bitWidth - i) + "0") ## bitsIn.takeLow(i)).asUInt) // delayed addr gen
      default(addr := U(0))
    }
  }

  symbolOut := qamRom.readSync(addr)
}

object AdaptiveQammod extends App {

  val sfix = HardType(SFix(2 exp, -15 exp))

  SimConfig.withFstWave.compile(AdaptiveQammod(Seq(2, 4, 6), sfix)).doSim { dut =>

    dut.clockDomain.forkStimulus(2)

    def testForOnce(alloc: Int) = {
      dut.orderIn #= (alloc - 1) // modulation order = 1 << 2 = 4
      val bitsIn = ChainsawRand.nextInt(1 << alloc)
      dut.bitsIn #= bitsIn // first symbol
      dut.clockDomain.waitSampling(3) // 2 + 1
      val golden = getSymbols(1 << alloc)(bitsIn) / getRms(1 << alloc)
      val yours = dut.symbolOut.toComplex
      assert(abs(golden - yours) < 1E-2)
    }

    (0 until 100).foreach(_ => testForOnce(6))
    (0 until 100).foreach(_ => testForOnce(4))
    (0 until 100).foreach(_ => testForOnce(2))

  }

  //  VivadoImpl(AdaptiveQammod(Seq(2, 4, 6), sfix))

}
