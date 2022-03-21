package Chainsaw.DFG

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
import org.slf4j.LoggerFactory

import scala.collection.JavaConversions._

object TransformTests {

  val logger = LoggerFactory.getLogger("DFG Transform Test Logger")

  def testConsistency[T <: Data](
      name: String,
      original: DFGGraph[T],
      transformed: DFGGraph[T],
      testWidth: Int,
      speedUp: Int,
      testLength: Int = 50,
      initLength: Int
  )(implicit holderProvider: HolderProvider[T]) {}

}
