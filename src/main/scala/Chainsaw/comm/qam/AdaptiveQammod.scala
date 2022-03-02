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