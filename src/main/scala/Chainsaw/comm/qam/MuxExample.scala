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

case class MuxExample() extends Component {

  val dataIn       = in Bits (1024 bits)
  val bitPositions = in Vec (UInt(10 bits), 256)
  val dataOut      = out Vec (Bits(7 bits), 256)

  dataOut.zip(bitPositions).zipWithIndex.foreach { case ((bits, position), i) =>
//      val candidate = dataIn(1023 downto i * 2)
    bits := dataIn(position, 7 bits)
  }
}

object MuxExample {
  def main(args: Array[String]): Unit = {
    VivadoSynth(MuxExample())
  }
}
