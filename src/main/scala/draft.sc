import Chainsaw.crypto.ReedSolomon._
import spinal.core._
import spinal.lib._
import scala.collection.mutable._

val rs = RS(15, 11)

val gx = rs.getGxCoff

val ab = new ArrayBuffer[Int]()

ab.contains(0)