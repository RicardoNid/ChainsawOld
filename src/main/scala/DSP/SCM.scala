package DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import xilinx.VivadoFlow

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Multiplierless Single Constant Multiplication
 *
 */
class SCM(constant: Int, bitWidth: Int, plain: Boolean = false) extends Component {

  val outputBitWidth = bitWidth + log2Up(constant + 1)

  val io = new Bundle {
    val input = slave Flow SInt(bitWidth bits)
    val output = master Flow SInt(outputBitWidth bits)
  }

  val result = SInt(outputBitWidth bits)

  //  adder tree 2-25 2-24 2-22
  //  reordered adder tree 2-25 2-24 2-19

  //  XILINX SYNTH,63
  if (plain) {
    result := (io.input.payload * constant).resized
    result.addAttribute("use_dsp = \"no\"")
  } else {
    val encoded = classicCSD(constant)

    val shifted = ArrayBuffer[SInt]()
    // start from msb
    (0 until encoded.length).foreach { i =>
      if (encoded(i) == '1') shifted += (io.input.payload << (encoded.length - 1 - i))
      else if (encoded(i) == '9') shifted += -(io.input.payload << (encoded.length - 1 - i))
    }

    result := AdderTree(shifted).resized
    result.simPublic()
  }

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

    val rand = new Random()
    val rands = (0 until 10).map(_ => rand.nextInt(4096))
    VivadoFlow(design = ???, topModuleName = ???, workspacePath = ???, vivadoConfig = ???, vivadoTask = ???, force = ???)
    val CSDReports = rands.map(r => VivadoFlow(design = new SCM(r, 16), topModuleName = "CSD_SCM", workspacePath = "output/SCM/CSD", force = true).doit())
    val plainReports = rands.map(r => VivadoFlow(design = new SCM(r, 16, plain = true), topModuleName = "CSD_SCM", workspacePath = "output/SCM/CSD", force = true).doit())

    val CSDAverage = CSDReports.map(_.LUT).sum.toDouble / 10
    val plainAverage = plainReports.map(_.LUT).sum.toDouble / 10

    println(s"CSD average = $CSDAverage")
    println(s"plain average = $plainAverage")
  }
}
