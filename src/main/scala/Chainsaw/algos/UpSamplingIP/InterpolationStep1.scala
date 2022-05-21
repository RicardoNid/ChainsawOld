package Chainsaw.algos.UpSamplingIP

import spinal.lib._
import spinal.core._
import spinal.lib.fsm._

case class InterpolationStep1(config: IPConfig) extends Component {

  def dW = config.dataW

  def sW = config.srcW

  def sH = config.srcH

  val io = new Bundle {
    // from master
    val dataIn       = slave Stream UInt(dW bits)
    val StartIn      = in Bool ()
    val frameStartIn = in Bool ()
    val rowEndIn     = in Bool ()

    // from slave
    val inComplete = in Bool ()

    // to slave
    val dataOut       = master Stream UInt(dW bits)
    val frameStartOut = out Bool ()
    val rowEndOut     = out Bool ()
    val inpValid      = out Bool ()

    // to master
    val inpComplete = out Bool ()

    // wait for axi-lite config signal
    val threshold = in UInt (dW bits)
    val widthIn   = in UInt (log2Up(sW + 1) bits)
    val heightIn  = in UInt (log2Up(sH + 1) bits)
  }
  noIoPrefix()

  /* set the initial state */
  io.setInvalid()

  /* register the frame start signal */
  val frameStart = RegNextWhen(io.frameStartIn, io.dataIn.fire).init(False)

  /* register the threshold */
  val inpThreshold = RegNext(io.threshold).init(U(0, dW bits))

  /* start buffer data signal */
  val holdBuffer = RegInit(False).clearWhen(io.StartIn.fall())

  /* the signal indicate last interpolation is complete */
  val interComplete = RegInit(False).setWhen(io.inComplete).clearWhen(io.StartIn.rise())

  /* when this signal is true, it means the nextRowBuffer is also lastRowBuffer */
  val sameBuffer = RegInit(False).clearWhen(io.StartIn.rise())

  /* record the number of row which is buffered */
  val bufferRowCount = Counter(sH + 1)

  /* when this signal is True, it means that the dataIn should be store in lineBufferTwo when we can receive dataIn*/
  val bufferSwitch = RegInit(False).clearWhen(io.StartIn.rise())

  /* the write enable signal of lineBuffer*/
  val bufferEnable = RegInit(False).setWhen(io.StartIn.rise())

  /* when it be true, it means the lineBufferTwo now is store the next row */
  val nextRowBuffer = RegInit(True).setWhen(io.StartIn.rise())

  /* address Counter for write data to buffer */
  val bufferWAddr = Counter(sW)

  /* the signal which indicate buffer can be flush */
  val flush = False

  /* address Counter for output pixels */
  val outPixelAddr = Counter(2 * sW)

  /* input bmp picture width */
  val bmpWidth = RegNext(io.widthIn).init(U(0, log2Up(sW + 1) bits))

  /* input bmp picture height */
  val bmpHeight = RegNext(io.heightIn).init(U(0, log2Up(sH + 1) bits))

  /* the number of row which is already output*/
  val outRowCount = Counter(2 * sH + 1)

  /* the following lineBuffer is used to store two line pixels of source bmp */
  val lineBufferOne = Mem(UInt(dW bits), sW).init(Seq.fill(sW)(U(0).resized))
  val lineBufferTwo = Mem(UInt(dW bits), sW).init(Seq.fill(sW)(U(0).resized))

  /* the register which is used to buffer the left side pixel of interpolating pixels */
  val leftPixel = RegInit(U(0, dW bits))

  /* diagonal diffs */
  val mainDiagDiff    = U(0, dW bits)
  val counterDiagDiff = U(0, dW bits)

  /* the holdBuffer and rowCount logic */
  when(bufferRowCount === bmpHeight - U(1) && bufferWAddr === bmpWidth - U(1) && io.dataIn.fire) {
    holdBuffer := True
    sameBuffer := True
  }

  when(io.rowEndIn && io.dataIn.fire) { bufferRowCount.increment() }

  /* the bufferSwitch and bufferEnable logic */
  when(io.rowEndIn && io.dataIn.fire) { bufferSwitch := ~bufferSwitch }
  when(io.StartIn && !holdBuffer) { io.dataIn.ready := bufferEnable }

  when(bufferRowCount >= U(1) && bufferWAddr === bmpWidth - U(1) && io.dataIn.fire && !flush) { bufferEnable := False }
  when(flush) {
    when(bufferRowCount =/= bmpHeight) {
      bufferEnable := True
    }
    nextRowBuffer := ~nextRowBuffer
  }

  /* write data to buffer logic */
  when(io.dataIn.fire) {
    lineBufferTwo.write(bufferWAddr, io.dataIn.payload, bufferEnable && bufferSwitch)
    lineBufferOne.write(bufferWAddr, io.dataIn.payload, bufferEnable && !bufferSwitch)
    when(io.rowEndIn) {
      bufferWAddr.clear()
    }.otherwise {
      bufferWAddr.increment()
    }
  }

  /* interpolate and output logic */

  val inpStateMachine = new StateMachine {
    val EVEN = StateEntryPoint()
    val ODD  = State()

    EVEN.onEntry { flush := True }
    EVEN.whenIsActive {
      when(outRowCount % U(2) === U(0) && outPixelAddr === U(2) * bmpWidth - U(1) && io.dataOut.fire) { goto(ODD) }

      when(frameStart && io.dataOut.valid) {
        io.frameStartOut := True
      }
      when(frameStart && io.dataOut.fire) {
        frameStart := False
      }
      when(outPixelAddr % U(2) === U(0) && (U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr)) {
        io.dataOut.valid := True
        switch(nextRowBuffer) {
          is(True) {
            io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
            leftPixel          := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
          }
          is(False) {
            io.dataOut.payload := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
            leftPixel          := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
          }
        }
      }.elsewhen(outPixelAddr % U(2) === U(1) && (U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > U(1) + outPixelAddr)) {
        io.dataOut.valid := True
        switch(nextRowBuffer) {
          is(True) {
            when(U(1) + outPixelAddr <= U(2) * bmpWidth - U(1)) {
              switch(leftPixel >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                is(True) {
                  when(leftPixel - lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) >= inpThreshold) {
                    io.dataOut.payload := leftPixel
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ leftPixel) / U(2)).resized
                  }
                }
                is(False) {
                  when(lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - leftPixel >= inpThreshold) {
                    io.dataOut.payload := leftPixel
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ leftPixel) / U(2)).resized
                  }
                }
              }
            }.otherwise {
              io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
            }
          }
          is(False) {
            when(U(1) + outPixelAddr <= U(2) * bmpWidth - U(1)) {
              switch(leftPixel >= lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                is(True) {
                  when(leftPixel - lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) >= inpThreshold) {
                    io.dataOut.payload := leftPixel
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ leftPixel) / U(2)).resized
                  }
                }
                is(False) {
                  when(lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - leftPixel >= inpThreshold) {
                    io.dataOut.payload := leftPixel
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ leftPixel) / U(2)).resized
                  }
                }
              }
            }.otherwise {
              io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
            }
          }
        }
      }
    }

    ODD.whenIsActive {
      when(outRowCount % U(2) === U(1) && outPixelAddr === U(2) * bmpWidth - U(1) && io.dataOut.fire) { goto(EVEN) }

      when(sameBuffer) {
        io.dataOut.valid := True
        when(outPixelAddr % U(2) === U(0)) {
          io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
        }.elsewhen(outPixelAddr % U(2) === U(1)) {
          switch(lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
            is(True) {
              mainDiagDiff := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)
            }
            is(False) {
              mainDiagDiff := lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
            }
          }

          when(mainDiagDiff >= inpThreshold) {
            io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
          }.otherwise {
            io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferOne.readAsync(
              ((U(1) + outPixelAddr) / U(2)).resized
            )) / U(2)).resized
          }
        }
      }.otherwise {
        when(outPixelAddr % U(2) === U(0) && (U(2) * bufferRowCount > U(1) + outRowCount || U(2) * bufferWAddr > outPixelAddr)) {
          io.dataOut.valid := True
          switch(lineBufferTwo.readAsync((outPixelAddr / U(2)).resized) >= lineBufferOne.readAsync((outPixelAddr / U(2)).resized)) {
            is(True) {
              when(lineBufferTwo.readAsync((outPixelAddr / U(2)).resized) - lineBufferOne.readAsync((outPixelAddr / U(2)).resized) >= inpThreshold) {
                switch(nextRowBuffer) {
                  is(True) {
                    io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
                  }
                  is(False) {
                    io.dataOut.payload := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
                  }
                }
              }.otherwise {
                io.dataOut.payload := ((lineBufferTwo.readAsync((outPixelAddr / U(2)).resized) +^ lineBufferOne.readAsync((outPixelAddr / U(2)).resized)) / U(
                  2
                )).resized
              }
            }
            is(False) {
              when(lineBufferOne.readAsync((outPixelAddr / U(2)).resized) - lineBufferTwo.readAsync((outPixelAddr / U(2)).resized) >= inpThreshold) {
                switch(nextRowBuffer) {
                  is(True) {
                    io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
                  }
                  is(False) {
                    io.dataOut.payload := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
                  }
                }
              }.otherwise {
                io.dataOut.payload := ((lineBufferTwo.readAsync((outPixelAddr / U(2)).resized) +^ lineBufferOne.readAsync((outPixelAddr / U(2)).resized)) / U(
                  2
                )).resized
              }
            }
          }
        }.elsewhen(outPixelAddr % U(2) === U(1) && (U(2) * bufferRowCount > U(1) + outRowCount || U(2) * bufferWAddr > U(1) + outPixelAddr)) {
          io.dataOut.valid := True
          when(U(1) + outPixelAddr <= U(2) * bmpWidth - U(1)) {
            switch(nextRowBuffer) {
              is(True) {
                switch(lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) >= lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                  is(True) {
                    mainDiagDiff := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferTwo.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )
                  }
                  is(False) {
                    mainDiagDiff := lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    )
                  }
                }
                switch(lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) >= lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)) {
                  is(True) {
                    counterDiagDiff := lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    )
                  }
                  is(False) {
                    counterDiagDiff := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferOne.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )
                  }
                }

                when(mainDiagDiff >= inpThreshold && counterDiagDiff >= inpThreshold) {
                  when(mainDiagDiff >= counterDiagDiff) {
                    io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                  }.otherwise {
                    io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                  }
                }.otherwise {
                  when(mainDiagDiff >= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferOne.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )) / U(2)).resized
                  }
                }
              }

              is(False) {
                switch(lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                  is(True) {
                    mainDiagDiff := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferOne.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )
                  }
                  is(False) {
                    mainDiagDiff := lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    )
                  }
                }
                switch(lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) >= lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)) {
                  is(True) {
                    counterDiagDiff := lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    )
                  }
                  is(False) {
                    counterDiagDiff := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferTwo.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )
                  }
                }

                when(mainDiagDiff >= inpThreshold && counterDiagDiff >= inpThreshold) {
                  when(mainDiagDiff >= counterDiagDiff) {
                    io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                  }.otherwise {
                    io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                  }
                }.otherwise {
                  when(mainDiagDiff >= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferOne.readAsync(
                      ((U(1) + outPixelAddr) / U(2)).resized
                    )) / U(2)).resized
                  }
                }
              }
            }
          }.otherwise {
            switch(lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) >= lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)) {
              is(True) {
                when(
                  lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) >= inpThreshold
                ) {
                  switch(nextRowBuffer) {
                    is(True) {
                      io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                    }
                    is(False) {
                      io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferOne.readAsync(
                    ((outPixelAddr - U(1)) / U(2)).resized
                  )) / U(2)).resized
                }
              }
              is(False) {
                when(
                  lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) >= inpThreshold
                ) {
                  switch(nextRowBuffer) {
                    is(True) {
                      io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                    }
                    is(False) {
                      io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferOne.readAsync(
                    ((outPixelAddr - U(1)) / U(2)).resized
                  )) / U(2)).resized
                }
              }
            }
          }
        }
      }
    }
  }

  when(io.dataOut.fire) {
    when(outPixelAddr === U(2) * bmpWidth - U(1)) {
      outPixelAddr.clear()
      when(outRowCount === U(2) * bmpHeight - U(1)) {
        outRowCount.clear()
        sameBuffer := False
        bufferRowCount.clear()
        bufferWAddr.clear()
        leftPixel.clearAll()
      }.otherwise {
        outRowCount.increment()
      }
    }.otherwise {
      outPixelAddr.increment()
    }
  }
  when(outPixelAddr === U(2) * bmpWidth - U(1) && io.dataOut.valid) {
    io.rowEndOut := True
  }

  io.inpValid.allowOverride
  io.inpComplete.allowOverride
  io.inpValid    := True
  io.inpComplete := interComplete
}

object GenTest extends App {
  SpinalVerilog(InterpolationStep1(IPConfig()))
}
