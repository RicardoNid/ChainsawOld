package Chainsaw.crypto.ReedSolomon

import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._

import cc.redberry.rings
import rings.poly.PolynomialMethods._
import rings.scaladsl._
import syntax._

class GaloisFieldHardWareTest extends AnyFlatSpec {
  case class GFMulti(m: Int) extends Component {
    val io = new Bundle {
      val dataIn  = in UInt (m bits)
      val dataOut = out Vec (UInt(m bits), 1 << m)
    }

    import GaloisFieldHardWare._
    io.dataOut.zipWithIndex.foreach { case (o, index) => o := io.dataIn.multi(index) }
  }
  "GF(16) constant multiplier" should "work normally" in simNow(4)
  "GF(32) constant multiplier" should "work normally" in simNow(5)


  def simNow(M: Int) ={
    SimConfig.withFstWave.compile(new GFMulti(M)).doSim { dut =>
      import dut._
      import PolynomialString._
      Range(0, 1 << M).foreach { i =>
        io.dataIn #= i
        sleep(1)
        io.dataOut.map(_.toInt).zipWithIndex.foreach { case (o, i) =>
          val gf   = GaloisField(M)
          val inGF = gf.getPoly(io.dataIn.toInt)
          val iGF  = gf.getPoly(i)
          val oGF  = inGF * iGF
          assert(gf.getInt(oGF) == o)
        }
      }
  }

  }

}
