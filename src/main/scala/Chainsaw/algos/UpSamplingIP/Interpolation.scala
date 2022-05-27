package Chainsaw.algos.UpSamplingIP

import spinal.core._
import spinal.lib._

case class Interpolation(config: IPConfig) extends Component {

  def dW = config.dataW

  def sW = config.srcW

  def sH = config.srcH

  val io = new Bundle {
    // from master
    val dataIn = slave Stream UInt(dW bits)
    val StartIn = in Bool()
    val frameStartIn = in Bool()
    val rowEndIn = in Bool()

    // to slave
    val dataOut = master Stream UInt(dW bits)
    val startOut = out Bool()
    val frameStartOut = out Bool()
    val rowEndOut = out Bool()
    val inpValidOut = out Bool()

    // to master
    val inpCompleteOut = out Bool()

    // wait for axi-lite config signal
    val thresholdIn = in UInt (dW bits)
    val widthIn = in UInt (log2Up(sW + 1) bits)
    val heightIn = in UInt (log2Up(sH + 1) bits)
  }

  noIoPrefix()

  val inpStep1 = InterpolationStep1(config)
  val inpStep2 = InterpolationStep2(config)
  val inpStep3 = InterpolationStep3(config)

  inpStep1.io.dataIn << io.dataIn
  inpStep1.io.StartIn := io.StartIn
  inpStep1.io.frameStartIn := io.frameStartIn
  inpStep1.io.rowEndIn := io.rowEndIn
  inpStep1.io.thresholdIn := io.thresholdIn
  inpStep1.io.widthIn := io.widthIn
  inpStep1.io.heightIn := io.heightIn
  inpStep1.io.inpTwoCompleteIn := inpStep2.io.inpTwoCompleteOut
  inpStep1.io.inpThreeCompleteIn := inpStep3.io.inpThreeCompleteOut

  io.inpCompleteOut := inpStep1.io.inpCompleteOut

  inpStep2.io.dataIn << inpStep1.io.dataOut
  inpStep2.io.StartIn := inpStep1.io.startOut
  inpStep2.io.frameStartIn := inpStep1.io.frameStartOut
  inpStep2.io.rowEndIn := inpStep1.io.rowEndOut
  inpStep2.io.inpValidIn := inpStep1.io.inpValidOut
  inpStep2.io.thresholdIn := io.thresholdIn
  inpStep2.io.widthIn := io.widthIn
  inpStep2.io.heightIn := io.heightIn
  inpStep2.io.inpThreeCompleteIn := inpStep3.io.inpThreeCompleteOut

  inpStep3.io.dataIn << inpStep2.io.dataOut
  inpStep3.io.StartIn := inpStep2.io.startOut
  inpStep3.io.frameStartIn := inpStep2.io.frameStartOut
  inpStep3.io.rowEndIn := inpStep2.io.rowEndOut
  inpStep3.io.inpValidIn := inpStep2.io.inpValidOut
  inpStep3.io.thresholdIn := io.thresholdIn
  inpStep3.io.widthIn := io.widthIn
  inpStep3.io.heightIn := io.heightIn

  io.dataOut << inpStep3.io.dataOut
  io.startOut := inpStep3.io.startOut
  io.frameStartOut := inpStep3.io.frameStartOut
  io.rowEndOut := inpStep3.io.rowEndOut
  io.inpValidOut := inpStep3.io.inpValidOut

}