package sysu.CNN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

package object huang {
  val dataWidth = 8

  def word_t = UInt(dataWidth bits)

  def double_word_t = UInt(2 * dataWidth bits)

  def output_t = UInt((2 * dataWidth + log2Up(64)) bits)

}
