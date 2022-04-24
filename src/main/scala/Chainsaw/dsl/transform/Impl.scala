package Chainsaw.dsl.transform

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class Impl(
                 size: (Int, Int),
                 impl: Vec[Bits] => Vec[Bits],
                 latency: Int
               )
