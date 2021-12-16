package Chainsaw.algos

import Chainsaw._
import Chainsaw.dspTest._
import breeze.numerics.abs
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._

import scala.language.postfixOps

class DivisionTest extends AnyFlatSpec {

  val testSize = 1000
  val Ns = (0 until testSize).map(_ => ChainsawRand.nextDouble())
  val Ds = (0 until testSize).map(_ => ChainsawRand.nextDouble() + 0.5) // range: [0.5,1.5)
  val golden = Ns.zip(Ds).map { case (n, d) => n / d }

  def fixType: SFix = SFix(2 exp, -13 exp) // SFix type for test

  behavior of "DivisionTest"

  it should "dbc" in Ns.zip(Ds).foreach { case (n, d) => Division.dbc(n, d, 10) }

  it should "dbcFixed" in {
    val dbcHardware: (SFix, SFix) => SFix = Division.dbcFixed(_, _, 10)
    val dutResults = evaluateCombBinary(dbcHardware, fixType, Ns, Ds)
    dutResults.zip(golden).forall { case (x, y) => abs(x - y) < 1E-4 }
  }
}
