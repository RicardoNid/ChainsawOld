
import spinal.core._
import spinal.lib._

package object MIPS_CPU {

  case class MemoryReadInterface(dataWidth: Int, addrWidth: Int) extends Bundle {
    val data = in UInt()
  }

}
