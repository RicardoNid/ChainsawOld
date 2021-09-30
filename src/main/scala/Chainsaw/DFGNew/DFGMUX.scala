package Chainsaw.DFGNew

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
import scala.collection.mutable.ArrayBuffer

import cc.redberry.rings.scaladsl._

case class Schedule(time: Int, period: Int)

case class DFGMUX[T <: Data](schedules: Seq[Seq[Schedule]])
                            (implicit holderProvider: BitCount => T) {
  def periodLcm = schedules.flatten.map(_.period).sorted.reverse.reduce(lcm(_, _))

  def occupationOf(schedule: Schedule) = (0 until periodLcm / schedule.period).map(_ * schedule.period + schedule.time)

  // check validity while init
  val allOccupations = schedules.flatten.map(occupationOf(_)).flatten
  require(allOccupations.size == allOccupations.distinct.size, "schedule collision") // no collision TODO: print collision location
  require(allOccupations.size == periodLcm) // lifetime is filled

  def impl(dataIns: Seq[T], count: UInt, globalLcm: Int) = {
    if (dataIns.size == 1) dataIns.head
    else {
      val multiple = globalLcm / periodLcm
      val ret = holderProvider(-1 bits)
      switch(count) {
        schedules.zip(dataIns).foreach { case (schedulesOneSource, bits) =>
          val occupationsOneSource = schedulesOneSource.map(occupationOf(_)).flatten
          val actualOccupations: Seq[Int] = (0 until multiple).map(i => occupationsOneSource.map(_ + i * periodLcm)).flatten
          println(actualOccupations.mkString(" "))
          is(actualOccupations.head, actualOccupations.tail: _*)(ret := bits)
        }
        default(ret := dataIns.head) // TODO: reconsider the "default" behavior
      }
      ret
    }
  }
}

object DFGMUX {
  def main(args: Array[String]): Unit = {
    GenRTL(new Component {
      val dataIn = in Vec(Bits(4 bits), 3)
      val count = in UInt (log2Up(24) bits)
      val dataOut = out Bits (4 bits)

      val mux = DFGMUX[Bits](Seq(
        Seq(Schedule(1, 4), Schedule(2, 4)),
        Seq(Schedule(0, 8), Schedule(4, 8)),
        Seq(Schedule(3, 12), Schedule(7, 12), Schedule(11, 12))
      ))

      dataOut := mux.impl(dataIn, count, 24)
    }, name = "complexMUX")

    GenRTL(new Component {
      val dataIn = in Bits (4 bits)
      val count = in UInt (log2Up(24) bits)
      val dataOut = out Bits (4 bits)

      val mux = DFGMUX[Bits](Seq(Seq(Schedule(1, 1))))
      dataOut := mux.impl(Seq(dataIn), count, 24)
    })
  }
}
