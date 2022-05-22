package Chainsaw.algos

import spinal.lib._
import spinal.core._

package object UpSamplingIP {
  case class IPConfig(srcH: Int = 540, srcW: Int = 960, dataW: Int = 8)

  implicit class InitUtils(port: Bundle) {
    def setInvalid(): Unit = {
      port.flatten.foreach {
        case bool: Bool =>
          if (bool.isOutput) bool := False
        case bitVector: BitVector =>
          if (bitVector.isOutput) bitVector match {
            case uint: UInt => uint := U(0).resized
            case _          =>
          }
        case _ =>
      }
    }
  }

}
