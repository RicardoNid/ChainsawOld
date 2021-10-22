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

import scala.collection.JavaConversions._

class Retiming[T](val dfg: DFGGraph[T], solution: Map[DSPNode[T], Int]) extends Transform {

  lazy val retimed: DFGGraph[T] = {
    val r = solution
    implicit val retimedDFG = dfg.clone().asInstanceOf[DFGGraph[T]]
    retimedDFG.foreachEdge { edge =>
      val (u, v) = (edge.source, edge.target)
      if (solution.contains(u) && solution.contains(v)) {
        retimedDFG.setEdgeWeight(edge, edge.weight + r(v) - r(u))
        // MUX transformation: new time = time + r(v)
        retimedDFG.setEdgeSchedules(edge, edge.schedules.map(schedule =>
          Schedule((schedule.time + r(v)) % schedule.period , schedule.period)))
      }
    }
    retimedDFG
  }

  override def latencyTransformations: Seq[LatencyTrans] = Seq(LatencyTrans(1, solution(dfg.outputNodes.head) - solution(dfg.inputNodes.head)))
}
