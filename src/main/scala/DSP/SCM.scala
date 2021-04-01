package DSP

import spinal.core._
import spinal.lib._
import xilinx.VivadoFlow

import scala.collection.mutable.ArrayBuffer

/** Multiplierless Single Constant Multiplication
 *
 */
class SCM(constant: Int, bitWidth: Int) extends Component {

  val outputBitWidth = bitWidth + log2Up(constant + 1)

  val io = new Bundle {
    val input = slave Flow SInt(bitWidth bits)
    val output = master Flow SInt(outputBitWidth bits)
  }

  val result = SInt(outputBitWidth bits)


  //  adder tree 2-25 2-24 2-22
      val encoded = classicCSD(constant)

      val shifted = ArrayBuffer[SInt]()
      (0 until encoded.length).foreach { i =>
        if (encoded(i) == '1') {
          println(i)
          shifted += (io.input.payload << (encoded.length - 1 - i))
        }
        else if (encoded(i) == '9') {
          println(i)
          shifted += -(io.input.payload << (encoded.length - 1 - i))
        }
      }

  val reorders =
      result := AdderTree(shifted).resized

  //  XILINX SYNTH,63
  //  result := (io.input.payload * constant).resized
  //  result.addAttribute("use_dsp = \"no\"")

  //  hard-coded CSD, 36 4-23 3-18
  //  result := ((io.input.payload << 7) - (io.input.payload << 5)) + (io.input.payload - (io.input.payload << 2))

  // hard-coded stepwise shift CSD 52 2-23 3-18*2
  //  result := (((io.input.payload << 2) - io.input.payload) << 5) + (io.input.payload - (io.input.payload << 2))

  //  hard-coded higher-order CSD 40 3-23 3-18
  //  val mid = (io.input.payload << 2) - io.input.payload
  //  result := (mid << 5) - mid


  io.output.payload := RegNext(result)
  io.output.valid := RegNext(io.input.valid)
  io.output.valid.init(False)
}

object SCM {
  def main(args: Array[String]): Unit = {

    val report1 = VivadoFlow(design = new SCM(93, 16), topModuleName = "CSD_SCM", workspacePath = "output/SCM/CSD", force = true).doit()
    report1.printArea
    report1.printFMax
  }
}
