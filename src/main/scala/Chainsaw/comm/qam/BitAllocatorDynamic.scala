package Chainsaw.comm.qam

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._
import scala.language.postfixOps

case class BitAllocatorDynamic(length: Int, num: Int, bitAvailable: Seq[Int]) extends Component {

  val dataIn = in Bits (length bits)

  val bitMin = bitAvailable.min
  val bitMax = bitAvailable.max

  //  val bitAllocIn = in Vec(UInt(log2Up(bitMax) bits), num)
  val bitPositionIn = in Vec(UInt(log2Up(length - 1) bits), num)
  val dataOut = out Vec(Bits(bitMax bits), num)

  // 1. raw implementation
  //  dataOut.zip(bitPositionIn).map { case (out, pos) => out := dataIn(pos, bitMax bits) }
  // 2. optimized by range
  dataOut.zip(bitPositionIn).zipWithIndex.foreach {
    case ((out, pos), i) =>
      val possibleMax = ((i + 1) * bitMax) min length
      val possibleMin = i * bitMin
      println(s"${possibleMax - 1} downto $possibleMin")
      val candidate = dataIn(possibleMax - 1 downto possibleMin)
      candidate.setName("candidate", weak = true)
      out := candidate(pos - possibleMin, bitMax bits)
  }
  // 3. optimized by RAM // TODO

}

object BitAllocatorDynamic {
  def main(args: Array[String]): Unit = {
    //    VivadoSynth(BitAllocatorDynamic(1024, 256, Seq(2, 3, 4, 5, 6, 7, 8)))
    SimConfig.withWave.compile(BitAllocatorDynamic(1024, 256, Seq(2, 3, 4, 5, 6, 7, 8))).doSim { dut =>
      dut.dataIn #= ChainsawRand.nextBigInt(1024)
      val bitAlloc = Seq.fill(64)(2) ++ Seq.fill(128)(4) ++ Seq.fill(64)(6)
      val bitPosition = (0 until 256).map(i => bitAlloc.take(i).sum)
      println(bitPosition.mkString(" "))
      dut.bitPositionIn.zip(bitPosition).foreach { case (port, pos) => port #= pos }
      sleep(1)
      println(dut.dataOut.map(_.toInt).mkString(" "))
    }

  }
}
