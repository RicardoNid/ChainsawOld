package projects.FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable

trait DSPSim {

  def init()

  def driver()

  def monitor()

  def scoreBoard()

}
