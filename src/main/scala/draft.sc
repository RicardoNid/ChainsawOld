import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import Chainsaw.examples.JsonExample.json
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import java.io.{File, PrintWriter, StringWriter}

val amplitude = 8.0

val x = (0 until 1000)
  .map(sin(_))
  .map(_ * amplitude + amplitude)
  .map(round(_))
  .map(BigInt(_).toString(16).padToLeft(2, '0'))
  .mkString(" ")

val sinFile = "/data/sin.txt"
val pw = new PrintWriter(new File(sinFile))
pw.write(x)
pw.close()

