package Chainsaw.algos.UpSamplingIP

import spinal.lib._
import spinal.core._
import spinal.lib.fsm._

case class InterpolationStep2(config: IPConfig) extends Component {

  def dW = config.dataW

  def sW = config.srcW

  def sH = config.srcH

  val io = new Bundle {
    // from master
    val dataIn       = slave Stream UInt(dW bits)
    val StartIn      = in Bool ()
    val frameStartIn = in Bool ()
    val rowEndIn     = in Bool ()
    val inpValidIn   = in Bool ()

    // from slave
    val inpThreeCompleteIn = in Bool ()

    // to slave
    val dataOut       = master Stream UInt(dW bits)
    val startOut      = out Bool ()
    val frameStartOut = out Bool ()
    val rowEndOut     = out Bool ()
    val inpValidOut   = out Bool ()

    // to master
    val inpTwoCompleteOut = out Bool ()

    // wait for axi-lite config signal
    val thresholdIn = in UInt (dW bits)
    val widthIn     = in UInt (log2Up(sW + 1) bits)
    val heightIn    = in UInt (log2Up(sH + 1) bits)
  }
  noIoPrefix()

  /* set the initial state */
  io.setInvalid()
  io.inpValidOut.allowOverride
  io.inpValidOut := True

  /* register the frame start signal */
  val frameStart = RegNextWhen(True, io.frameStartIn).init(False)

  /* register the startOut signal */
  val slaveStart = RegInit(False).setWhen(io.dataIn.fire && !io.inpThreeCompleteIn).clearWhen(io.inpThreeCompleteIn)

  /* register the threshold */
  val inpThreshold = RegNext(io.thresholdIn).init(U(128, dW bits))

  /* start buffer data signal */
  val holdBuffer = RegInit(False).clearWhen(io.StartIn.fall())

  /* the signal indicate last interpolation is complete */
  val interComplete = RegInit(False).clearWhen(io.StartIn.rise())

  /* when this signal is true, it means the nextRowBuffer is also lastRowBuffer */
  val sameBuffer = RegInit(False).clearWhen(io.StartIn.rise())

  /* record the number of row which is buffered */
  val bufferRowCount = Counter(2 * sH + 1)

  /* control which buffer will be write */
  val bufferSwitch = RegNextWhen(U(0, 2 bits), io.StartIn.rise()).init(U(0, 2 bits))

  /* the write enable signal of lineBuffer*/
  val bufferEnable = RegInit(False).setWhen(io.StartIn.rise())

  /* when it be true, it means the lineBufferTwo now is store the next row */
  val nextRowBuffer = RegInit(True).setWhen(io.StartIn.rise())

  /* address Counter for write data to buffer */
  val bufferWAddr = Counter(2 * sW)

  /* the signal which indicate buffer can be flush */
  val flush = False

  /* address Counter for output pixels */
  val outPixelAddr = Counter(4 * sW)

  /* input bmp picture width */
  val bmpWidth = RegNext(io.widthIn).init(U(sW, log2Up(sW + 1) bits))

  /* input bmp picture height */
  val bmpHeight = RegNext(io.heightIn).init(U(sH, log2Up(sH + 1) bits))

  /* the number of row which is already output*/
  val outRowCount = Counter(4 * sH + 1)

  /* the following lineBuffer is used to store two line pixels of source bmp */
  val lineBufferOne = Mem(UInt(dW bits), 2 * sW).init(Seq.fill(2 * sW)(U(0).resized))
  val lineBufferTwo = Mem(UInt(dW bits), 2 * sW).init(Seq.fill(2 * sW)(U(0).resized))

  /* the lineBuffer for Odd row of dataIn */
  val oddLineBuffer = Mem(UInt(dW bits), 2 * sW).init(Seq.fill(2 * sW)(U(0).resized))

  /* diagonal diffs */
  val mainDiagDiff    = U(0, dW bits)
  val counterDiagDiff = U(0, dW bits)

  /* the holdBuffer and rowCount logic */
  when(bufferRowCount === U(2) * bmpHeight - U(1) && bufferWAddr === U(2) * bmpWidth - U(1) && io.dataIn.fire) {
    holdBuffer := True
    sameBuffer := True
  }

  when(io.rowEndIn && io.dataIn.fire) { bufferRowCount.increment() }

  /* the bufferSwitch and bufferEnable logic */
  when(io.rowEndIn && io.dataIn.fire) {
    when(bufferSwitch === U(2) || bufferSwitch === U(0)) {
      bufferSwitch := U(1, 2 bits)
    }.otherwise {
      when(nextRowBuffer) {
        bufferSwitch := bufferSwitch + U(1)
      }.otherwise {
        bufferSwitch := bufferSwitch - U(1)
      }

    }
  }
  when(io.StartIn && !holdBuffer) { io.dataIn.ready := bufferEnable }

  when(bufferRowCount >= U(2) && bufferRowCount % U(2) === U(0) && bufferWAddr === U(2) * bmpWidth - U(1) && io.dataIn.fire && !flush) { bufferEnable := False }
  when(flush) {
    when(bufferRowCount =/= U(2) * bmpHeight) {
      bufferEnable := True
    }
    nextRowBuffer := ~nextRowBuffer
  }

  /* write data to buffer logic */
  when(io.dataIn.fire) {
    lineBufferOne.write(bufferWAddr, io.dataIn.payload, bufferEnable && bufferSwitch === U(0))
    oddLineBuffer.write(bufferWAddr, io.dataIn.payload, bufferEnable && bufferSwitch === U(1))
    lineBufferTwo.write(bufferWAddr, io.dataIn.payload, bufferEnable && bufferSwitch === U(2))
    when(io.rowEndIn) {
      bufferWAddr.clear()
    }.otherwise {
      bufferWAddr.increment()
    }
  }

  val inpStateMachine = new StateMachine {
    val FIRST                 = StateEntryPoint()
    val SECOND, THIRD, FOURTH = State()

    FIRST.onEntry {
      when(outRowCount =/= U(0)) {
        flush := True
      }
    }
    FIRST.whenIsActive {
      when(outRowCount % U(4) === U(0) && outPixelAddr === U(4) * bmpWidth - U(1) && io.dataOut.fire) { goto(SECOND) }

      when(frameStart && io.dataOut.valid) {
        io.frameStartOut := True
      }
      when(frameStart && io.dataOut.fire) {
        frameStart := False
      }

      switch(outPixelAddr % U(4)) {
        is(U(0)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
              }
            }
          }
        }
        is(U(1)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
            }
          }
        }
        is(U(2)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
              }
            }
          }
        }
        is(U(3)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > U(1) + outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
              is(False) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
            }
          }
        }
      }
    }

    SECOND.whenIsActive {
      when(outRowCount % U(2) === U(1) && outPixelAddr === U(4) * bmpWidth - U(1) && io.dataOut.fire) { goto(THIRD) }

      switch(outPixelAddr % U(4)) {
        is(U(0)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
              }
            }
          }
        }
        is(U(1)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
            }
          }
        }
        is(U(2)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(2)) / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(2)) / U(2)).resized)
              }
            }
          }
        }
        is(U(3)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > U(1) + outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
              is(False) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
            }
          }
        }
      }
    }

    THIRD.whenIsActive {
      when(outRowCount % U(2) === U(2) && outPixelAddr === U(4) * bmpWidth - U(1) && io.dataOut.fire) { goto(FOURTH) }

      switch(outPixelAddr % U(4)) {
        is(U(0)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := oddLineBuffer.readAsync((outPixelAddr / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := oddLineBuffer.readAsync((outPixelAddr / U(2)).resized)
              }
            }
          }
        }
        is(U(1)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
            }
          }
        }
        is(U(2)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := oddLineBuffer.readAsync((outPixelAddr / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := oddLineBuffer.readAsync((outPixelAddr / U(2)).resized)
              }
            }
          }
        }
        is(U(3)) {
          when(U(2) * bufferRowCount > outRowCount || U(2) * bufferWAddr > U(1) + outPixelAddr) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
              is(False) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
            }
          }
        }
      }
    }

    FOURTH.whenIsActive {
      when(outRowCount % U(2) === U(3) && outPixelAddr === U(4) * bmpWidth - U(1) && io.dataOut.fire) { goto(FIRST) }

      when(!sameBuffer) {
        switch(outPixelAddr % U(4)) {
          is(U(0)) {
            when(U(2) * bufferRowCount > U(1) + outRowCount || U(2) * bufferWAddr > outPixelAddr) {
              io.dataOut.valid := True
              switch(lineBufferOne.readAsync((outPixelAddr / U(2)).resized) >= lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)) {
                is(True) {
                  when(
                    lineBufferOne.readAsync((outPixelAddr / U(2)).resized) - lineBufferTwo.readAsync(
                      (outPixelAddr / U(2)).resized
                    ) >= inpThreshold
                  ) {
                    io.dataOut.payload := U(0, dW bits)
                    io.inpValidOut     := False
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync((outPixelAddr / U(2)).resized) +^ lineBufferTwo.readAsync(
                      (outPixelAddr / U(2)).resized
                    )) / U(2)).resized
                  }
                }
                is(False) {
                  when(
                    lineBufferTwo.readAsync((outPixelAddr / U(2)).resized) - lineBufferOne.readAsync(
                      (outPixelAddr / U(2)).resized
                    ) >= inpThreshold
                  ) {
                    io.dataOut.payload := U(0, dW bits)
                    io.inpValidOut     := False
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync((outPixelAddr / U(2)).resized) +^ lineBufferOne.readAsync(
                      (outPixelAddr / U(2)).resized
                    )) / U(2)).resized
                  }
                }
              }
            }
          }
          is(U(1)) {
            when(U(2) * bufferRowCount > U(1) + outRowCount || U(2) * bufferWAddr > outPixelAddr) {
              io.dataOut.valid := True
              switch(lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) >= lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)) {
                is(True) {
                  when(
                    lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferTwo.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    ) >= inpThreshold
                  ) {
                    io.dataOut.payload := U(0, dW bits)
                    io.inpValidOut     := False
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    )) / U(2)).resized
                  }
                }
                is(False) {
                  when(
                    lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) - lineBufferOne.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    ) >= inpThreshold
                  ) {
                    io.dataOut.payload := U(0, dW bits)
                    io.inpValidOut     := False
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized) +^ lineBufferOne.readAsync(
                      ((outPixelAddr - U(1)) / U(2)).resized
                    )) / U(2)).resized
                  }
                }
              }
            }
          }
          is(U(2)) {
            when(U(2) * bufferRowCount > U(1) + outRowCount || U(2) * bufferWAddr > outPixelAddr) {
              io.dataOut.valid := True
              switch(lineBufferOne.readAsync(((outPixelAddr - U(2)) / U(2)).resized) >= lineBufferTwo.readAsync(((outPixelAddr - U(2)) / U(2)).resized)) {
                is(True) {
                  when(
                    lineBufferOne.readAsync(((outPixelAddr - U(2)) / U(2)).resized) - lineBufferTwo.readAsync(
                      ((outPixelAddr - U(2)) / U(2)).resized
                    ) >= inpThreshold
                  ) {
                    io.dataOut.payload := U(0, dW bits)
                    io.inpValidOut     := False
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(2)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                      ((outPixelAddr - U(2)) / U(2)).resized
                    )) / U(2)).resized
                  }
                }
                is(False) {
                  when(
                    lineBufferTwo.readAsync(((outPixelAddr - U(2)) / U(2)).resized) - lineBufferOne.readAsync(
                      ((outPixelAddr - U(2)) / U(2)).resized
                    ) >= inpThreshold
                  ) {
                    io.dataOut.payload := U(0, dW bits)
                    io.inpValidOut     := False
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(2)) / U(2)).resized) +^ lineBufferOne.readAsync(
                      ((outPixelAddr - U(2)) / U(2)).resized
                    )) / U(2)).resized
                  }
                }
              }
            }
          }
          is(U(3)) {
            when(U(2) * bufferRowCount > U(1) + outRowCount || U(2) * bufferWAddr > U(1) + outPixelAddr) {
              io.dataOut.valid := True
              switch(nextRowBuffer) {
                is(True) {
                  when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                    switch(lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                      is(True) {
                        mainDiagDiff := lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )
                      }
                      is(False) {
                        mainDiagDiff := lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )
                      }
                    }
                    switch(lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) >= lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized)) {
                      is(True) {
                        counterDiagDiff := lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )
                      }
                      is(False) {
                        counterDiagDiff := lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )
                      }
                    }

                    when(mainDiagDiff >= inpThreshold && counterDiagDiff >= inpThreshold) {
                      io.dataOut.payload := U(0, dW bits)
                      io.inpValidOut     := False
                    }.otherwise {
                      when(mainDiagDiff >= counterDiagDiff) {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }.otherwise {
                    switch(lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized)) {
                      is(True) {
                        when(
                          lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferTwo.readAsync(
                            ((outPixelAddr - U(3)) / U(2)).resized
                          ) >= inpThreshold
                        ) {
                          io.dataOut.payload := U(0, dW bits)
                          io.inpValidOut     := False
                        }.otherwise {
                          io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                            ((outPixelAddr - U(3)) / U(2)).resized
                          )) / U(2)).resized
                        }
                      }
                      is(False) {
                        when(
                          lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                            ((outPixelAddr - U(3)) / U(2)).resized
                          ) >= inpThreshold
                        ) {
                          io.dataOut.payload := U(0, dW bits)
                          io.inpValidOut     := False
                        }.otherwise {
                          io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                            ((outPixelAddr - U(3)) / U(2)).resized
                          )) / U(2)).resized
                        }
                      }
                    }
                  }
                }
                is(False) {
                  when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                    switch(lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                      is(True) {
                        mainDiagDiff := lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )
                      }
                      is(False) {
                        mainDiagDiff := lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )
                      }
                    }
                    switch(lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) >= lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized)) {
                      is(True) {
                        counterDiagDiff := lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )
                      }
                      is(False) {
                        counterDiagDiff := lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )
                      }
                    }

                    when(mainDiagDiff >= inpThreshold && counterDiagDiff >= inpThreshold) {
                      io.dataOut.payload := U(0, dW bits)
                      io.inpValidOut     := False
                    }.otherwise {
                      when(mainDiagDiff >= counterDiagDiff) {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }.otherwise {
                    switch(lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized)) {
                      when(
                        lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }.otherwise {
        switch(outPixelAddr % U(4)) {
          is(U(0)) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync((outPixelAddr / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync((outPixelAddr / U(2)).resized)
              }
            }
          }
          is(U(1)) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(1)) / U(2)).resized)
              }
            }
          }
          is(U(2)) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(2)) / U(2)).resized)
              }
              is(False) {
                io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(2)) / U(2)).resized)
              }
            }
          }
          is(U(3)) {
            io.dataOut.valid := True
            switch(nextRowBuffer) {
              is(True) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferOne.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferOne.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferOne.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
              is(False) {
                when(U(1) + outPixelAddr <= U(4) * bmpWidth - U(1)) {
                  switch(lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) >= lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized)) {
                    is(True) {
                      when(
                        lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((U(1) + outPixelAddr) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                    is(False) {
                      when(
                        lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) - lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        ) >= inpThreshold
                      ) {
                        io.dataOut.payload := U(0, dW bits)
                        io.inpValidOut     := False
                      }.otherwise {
                        io.dataOut.payload := ((lineBufferTwo.readAsync(((U(1) + outPixelAddr) / U(2)).resized) +^ lineBufferTwo.readAsync(
                          ((outPixelAddr - U(3)) / U(2)).resized
                        )) / U(2)).resized
                      }
                    }
                  }
                }.otherwise {
                  io.dataOut.payload := lineBufferTwo.readAsync(((outPixelAddr - U(3)) / U(2)).resized)
                }
              }
            }
          }
        }
      }
    }

    when(io.dataOut.fire) {
      when(outPixelAddr === U(4) * bmpWidth - U(1)) {
        outPixelAddr.clear()
        when(outRowCount === U(4) * bmpHeight - U(1)) {
          outRowCount.clear()
          sameBuffer := False
          bufferRowCount.clear()
          bufferWAddr.clear()
          interComplete := True
        }.otherwise {
          outRowCount.increment()
        }
      }.otherwise {
        outPixelAddr.increment()
      }
    }
    when(outPixelAddr === U(4) * bmpWidth - U(1) && io.dataOut.valid) {
      io.rowEndOut := True
    }
    when(interComplete) { interComplete := False }

    io.inpTwoCompleteOut.allowOverride
    io.startOut.allowOverride
    io.inpTwoCompleteOut := interComplete
    io.startOut          := slaveStart

  }

}
