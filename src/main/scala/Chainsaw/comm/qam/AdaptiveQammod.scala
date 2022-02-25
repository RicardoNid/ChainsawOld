package Chainsaw.comm.qam

import Chainsaw._
import spinal.core._

import scala.language.postfixOps
import algos.Qam.{getSymbols, getRms}

case class AdaptiveQammod(bitCandidates: Seq[Int], hardType: HardType[SFix]) extends Component {

  val bitWidth = bitCandidates.max

  val orderIn = in UInt (log2Up(bitCandidates.max) bits) // modulation order
  val bitsIn = in Bits (bitWidth bits)
  val symbolOut = out(ComplexNumber(hardType))

  // building rom
  val orders = (bitCandidates.min to bitWidth).map(1 << _)
  val qamValues = orders.flatMap(order => getSymbols(order).toArray.map(_ / getRms(order))) // get normalized symbols
  val padding = Seq.fill(1 << bitCandidates.min)(BComplex(0.0, 0.0))
  val qamRom = Mem((padding ++ qamValues).map(CN(_, hardType)))

  // addr generator
  val addr = qamRom.addressType()
  switch(orderIn) {
    bitCandidates.foreach { i =>
      is(U(i))(addr := RegNext(B("1" * (bitWidth + 1 - i)) ## bitsIn.takeLow(i)).asUInt) // delayed addr gen
      default(addr := U(0))
    }
  }

  symbolOut := qamRom.readSync(addr)
}

object AdaptiveQammod extends App {
  val sfix = HardType(SFix(2 exp, -15 exp))
  VivadoImpl(AdaptiveQammod(Seq(2, 4, 6), sfix))
}
