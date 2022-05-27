package Chainsaw.algos.UpSamplingIP

import spinal.core._
import spinal.lib._

case class InterpolationStep3(config: IPConfig) extends Component {
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

    // to slave
    val dataOut       = master Stream UInt(dW bits)
    val startOut      = out Bool ()
    val frameStartOut = out Bool ()
    val rowEndOut     = out Bool ()
    val inpValidOut   = out Bool ()

    // to master
    val inpThreeCompleteOut = out Bool ()

    // wait for axi-lite config signal
    val thresholdIn = in UInt (dW bits)
    val widthIn     = in UInt (log2Up(sW + 1) bits)
    val heightIn    = in UInt (log2Up(sH + 1) bits)
  }
  noIoPrefix()

  /* set the initial state */
  io.setInvalid()

  /* register the frame start signal */
  val frameStart = RegNextWhen(True, io.frameStartIn).init(False)

  /* start buffer data signal */
  val holdBuffer = RegInit(False).clearWhen(io.StartIn.fall())

  /* the signal indicate last interpolation is complete */
  val interComplete = RegInit(False).clearWhen(io.StartIn.rise())

  /* register the startOut signal */
  val slaveStart = RegInit(False).setWhen(io.dataIn.fire).clearWhen(interComplete)

//  /* when this signal is true, it means the nextRowBuffer is also lastRowBuffer */
//  val sameBuffer = RegInit(False).clearWhen(io.StartIn.rise())

  /* record the number of row which is buffered */
  val bufferRowCount = Counter(4 * sH + 1)

  /* control which buffer will be write */
  val bufferSwitch = RegNextWhen(U(0, 2 bits), io.StartIn.rise()).init(U(0, 2 bits))

  /* the write enable signal of lineBuffer*/
  val bufferEnable = RegInit(False).setWhen(io.StartIn.rise())

  /* indicate the lineBuffer location information in image */
  val firstRowBuffer = RegInit(U(0, 2 bits))
  val nextRowBuffer  = RegInit(U(1, 2 bits))
//  val lastRowBuffer  = RegInit(U(2, 2 bits))

  /* address Counter for write data to buffer */
  val bufferWAddr = Counter(4 * sW)

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

  /* the following lineBuffer is used to store three line pixels of source bmp */
  val lineBufferOne   = Mem(UInt(dW bits), 4 * sW).init(Seq.fill(4 * sW)(U(0).resized))
  val lineBufferTwo   = Mem(UInt(dW bits), 4 * sW).init(Seq.fill(4 * sW)(U(0).resized))
  val lineBufferThree = Mem(UInt(dW bits), 4 * sW).init(Seq.fill(4 * sW)(U(0).resized))

  /* the following validBuffer is used to store three line valid interpolation signal of source bmp */
  val validBufferOne   = Mem(Bool(), 4 * sW).init(Seq.fill(4 * sW)(True))
  val validBufferTwo   = Mem(Bool(), 4 * sW).init(Seq.fill(4 * sW)(True))
  val validBufferThree = Mem(Bool(), 4 * sW).init(Seq.fill(4 * sW)(True))
  /* diagonal diffs */
  val mainDiagDiff    = U(0, dW bits)
  val counterDiagDiff = U(0, dW bits)
  val horizDiff       = U(0, dW bits)
  val vertDiff        = U(0, dW bits)

  /* initial buffer indicate signal */
  when(io.StartIn.rise()) {
    firstRowBuffer := U(0, 2 bits)
    nextRowBuffer  := U(1, 2 bits)
//    lastRowBuffer  := U(2, 2 bits)
  }

  /* the holdBuffer and rowCount logic */
  when(bufferRowCount === U(4) * bmpHeight - U(1) && bufferWAddr === U(4) * bmpWidth - U(1) && io.dataIn.fire) {
    holdBuffer := True
//    sameBuffer := True
  }

  when(io.rowEndIn && io.dataIn.fire) { bufferRowCount.increment() }

  /* the bufferSwitch and bufferEnable logic */
  when(io.rowEndIn && io.dataIn.fire) {
    when(bufferSwitch === U(2)) {
      bufferSwitch := U(0, 2 bits)
    }.otherwise {
      bufferSwitch := bufferSwitch + U(1)
    }
  }
  when(io.StartIn && !holdBuffer) { io.dataIn.ready := bufferEnable }

  when(bufferRowCount >= U(2) && bufferWAddr === U(4) * bmpWidth - U(1) && io.dataIn.fire && !flush) { bufferEnable := False }
  when(flush) {
    when(bufferRowCount =/= U(4) * bmpHeight) {
      bufferEnable := True
    }
    when(firstRowBuffer === U(2)) {
      firstRowBuffer := U(0, 2 bits)
    }.otherwise {
      firstRowBuffer := firstRowBuffer + U(1)
    }
    when(nextRowBuffer === U(2)) {
      nextRowBuffer := U(0, 2 bits)
    }.otherwise {
      nextRowBuffer := nextRowBuffer + U(1)
    }
//    when(lastRowBuffer === U(2)) {
//      lastRowBuffer := U(0, 2 bits)
//    }.otherwise {
//      lastRowBuffer := lastRowBuffer + U(1)
//    }
  }
  when(outRowCount >= U(1) && outPixelAddr === U(4) * bmpWidth - U(1) && io.dataOut.fire){
    flush := True
  }

  /* write data to buffer logic */
  when(io.dataIn.fire) {
    lineBufferOne.write(bufferWAddr, io.dataIn.payload, bufferEnable && bufferSwitch === U(0))
    lineBufferTwo.write(bufferWAddr, io.dataIn.payload, bufferEnable && bufferSwitch === U(1))
    lineBufferThree.write(bufferWAddr, io.dataIn.payload, bufferEnable && bufferSwitch === U(2))

    validBufferOne.write(bufferWAddr, io.inpValidIn, bufferEnable && bufferSwitch === U(0))
    validBufferTwo.write(bufferWAddr, io.inpValidIn, bufferEnable && bufferSwitch === U(1))
    validBufferThree.write(bufferWAddr, io.inpValidIn, bufferEnable && bufferSwitch === U(2))

    when(io.rowEndIn) {
      bufferWAddr.clear()
    }.otherwise {
      bufferWAddr.increment()
    }
  }

  when(outRowCount < U(4) * bmpHeight - 1) {
    when(bufferRowCount > U(1) + outRowCount || (bufferRowCount === U(1) + outRowCount && bufferWAddr > U(1) + outPixelAddr)) {
      io.dataOut.valid := True
      when(outRowCount === U(0)) {
        when(outPixelAddr === U(0)) {
          when(validBufferOne.readAsync(outPixelAddr)) {
            io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
          }.otherwise {
            when(validBufferOne.readAsync(U(1) + outPixelAddr)) {
              io.dataOut.payload := lineBufferOne.readAsync(U(1) + outPixelAddr)
            }.otherwise {
              when(validBufferTwo.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
              }.otherwise {
                io.dataOut.payload := lineBufferTwo.readAsync(U(1) + outPixelAddr)
              }
            }
          }
        }.elsewhen(outPixelAddr === U(4) * bmpWidth - U(1)) {
          when(validBufferOne.readAsync(outPixelAddr)) {
            io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
          }.otherwise {
            when(validBufferOne.readAsync(outPixelAddr - U(1))) {
              io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr - U(1))
            }.otherwise {
              when(validBufferTwo.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
              }.otherwise {
                io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr - U(1))
              }
            }
          }
        }.otherwise {
          when(validBufferOne.readAsync(outPixelAddr)){
            io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
          }.otherwise{
            when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferOne.readAsync(U(1) + outPixelAddr)) {
              when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferOne.readAsync(U(1) + outPixelAddr)) {
                horizDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
              }.otherwise {
                horizDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
              }
            }.otherwise {
              horizDiff.setAll()
            }

            when(validBufferTwo.readAsync(outPixelAddr)) {
              vertDiff.clearAll()
            }.otherwise {
              vertDiff.setAll()
            }
            when(validBufferTwo.readAsync(outPixelAddr - U(1)) && validBufferTwo.readAsync(U(1) + outPixelAddr)) {
              when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                mainDiagDiff    := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                counterDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
              }.otherwise {
                mainDiagDiff    := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                counterDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
              }
            }.otherwise {
              mainDiagDiff.setAll()
              counterDiagDiff.setAll()
            }
            when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
              io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
            }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
              io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
            }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
              io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
            }.otherwise {
              io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
            }
          }
        }
      }.elsewhen(outRowCount === U(1)) {
        when(outPixelAddr === U(0)) {
          when(validBufferTwo.readAsync(outPixelAddr)) {
            io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
          }.otherwise {
            when(validBufferTwo.readAsync(U(1) + outPixelAddr)) {
              io.dataOut.payload := lineBufferTwo.readAsync(U(1) + outPixelAddr)
            }.otherwise {
              horizDiff.setAll()
              when(validBufferOne.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                  vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                }.otherwise {
                  vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                }
              }.otherwise {
                vertDiff.setAll()
              }

              when(validBufferOne.readAsync(U(1) + outPixelAddr) && validBufferThree.readAsync(U(1) + outPixelAddr)) {
                when(lineBufferOne.readAsync(U(1) + outPixelAddr) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                  mainDiagDiff    := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                  counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                }.otherwise {
                  mainDiagDiff    := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                  counterDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                }
              }.otherwise {
                mainDiagDiff.setAll()
                counterDiagDiff.setAll()
              }

              when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
              }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
              }.otherwise {
                io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
              }
            }
          }
        }.elsewhen(outPixelAddr === U(4) * bmpWidth - U(1)) {
          when(validBufferTwo.readAsync(outPixelAddr)) {
            io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
          }.otherwise {
            when(validBufferTwo.readAsync(outPixelAddr - U(1))) {
              io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr - U(1))
            }.otherwise {
              horizDiff.setAll()
              when(validBufferOne.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                  vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                }.otherwise {
                  vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                }
              }.otherwise {
                vertDiff.setAll()
              }

              when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferThree.readAsync(outPixelAddr - U(1))) {
                when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(outPixelAddr - U(1))) {
                  mainDiagDiff    := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(outPixelAddr - U(1))
                  counterDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(outPixelAddr - U(1))
                }.otherwise {
                  mainDiagDiff    := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(outPixelAddr - U(1))
                  counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(outPixelAddr - U(1))
                }
              }.otherwise {
                mainDiagDiff.setAll()
                counterDiagDiff.setAll()
              }

              when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
              }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
              }.otherwise {
                io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
              }
            }
          }
        }.otherwise {
          when(validBufferTwo.readAsync(outPixelAddr)){
            io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
          }.otherwise{
            when(validBufferTwo.readAsync(outPixelAddr - U(1)) && validBufferTwo.readAsync(U(1) + outPixelAddr)) {
              when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                horizDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
              }.otherwise {
                horizDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
              }
            }.otherwise {
              horizDiff.setAll()
            }

            when(validBufferOne.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
              when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
              }.otherwise {
                vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
              }
            }.otherwise {
              vertDiff.setAll()
            }

            when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferThree.readAsync(U(1) + outPixelAddr)) {
              when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                mainDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
              }.otherwise {
                mainDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
              }
            }.otherwise {
              mainDiagDiff.setAll()
            }

            when(validBufferOne.readAsync(U(1) + outPixelAddr) && validBufferThree.readAsync(outPixelAddr - U(1))) {
              when(lineBufferOne.readAsync(U(1) + outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr - U(1))) {
                counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
              }.otherwise {
                counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
              }
            }.otherwise {
              counterDiagDiff.setAll()
            }

            when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
              io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
            }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
              io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
            }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
              io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
            }.otherwise {
              io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
            }
          }
        }
      }.otherwise {
        when(outPixelAddr === U(0)) {
          switch(nextRowBuffer){
            is(U(0)){
              when(validBufferOne.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
              }.otherwise {
                when(validBufferOne.readAsync(U(1) + outPixelAddr)) {
                  io.dataOut.payload := lineBufferOne.readAsync(U(1) + outPixelAddr)
                }.otherwise {
                  horizDiff.setAll()
                  when(validBufferTwo.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                    when(lineBufferTwo.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                      vertDiff := lineBufferTwo.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                    }.otherwise {
                      vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr)
                    }
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferTwo.readAsync(U(1) + outPixelAddr) && validBufferThree.readAsync(U(1) + outPixelAddr)) {
                    when(lineBufferTwo.readAsync(U(1) + outPixelAddr) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }

                  when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                  }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }
            }

            is(U(1)){
              when(validBufferTwo.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
              }.otherwise {
                when(validBufferTwo.readAsync(U(1) + outPixelAddr)) {
                  io.dataOut.payload := lineBufferTwo.readAsync(U(1) + outPixelAddr)
                }.otherwise {
                  horizDiff.setAll()
                  when(validBufferOne.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                    when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                      vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                    }.otherwise {
                      vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                    }
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferOne.readAsync(U(1) + outPixelAddr) && validBufferThree.readAsync(U(1) + outPixelAddr)) {
                    when(lineBufferOne.readAsync(U(1) + outPixelAddr) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }

                  when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                  }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }
            }

            is(U(2)){
              when(validBufferThree.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
              }.otherwise {
                when(validBufferThree.readAsync(U(1) + outPixelAddr)) {
                  io.dataOut.payload := lineBufferThree.readAsync(U(1) + outPixelAddr)
                }.otherwise {
                  horizDiff.setAll()
                  when(validBufferOne.readAsync(outPixelAddr) && validBufferTwo.readAsync(outPixelAddr)) {
                    when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferTwo.readAsync(outPixelAddr)) {
                      vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr)
                    }.otherwise {
                      vertDiff := lineBufferTwo.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                    }
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferOne.readAsync(U(1) + outPixelAddr) && validBufferTwo.readAsync(U(1) + outPixelAddr)) {
                    when(lineBufferOne.readAsync(U(1) + outPixelAddr) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }

                  when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferTwo.readAsync(outPixelAddr)) / U(2)).resized
                  }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }
            }
          }
        }.elsewhen(outPixelAddr === U(4) * bmpWidth - U(1)) {
          switch(nextRowBuffer){
            is(U(0)){
              when(validBufferOne.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
              }.otherwise {
                when(validBufferOne.readAsync(outPixelAddr - U(1))) {
                  io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr - U(1))
                }.otherwise {
                  horizDiff.setAll()
                  when(validBufferTwo.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                    when(lineBufferTwo.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                      vertDiff := lineBufferTwo.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                    }.otherwise {
                      vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr)
                    }
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferTwo.readAsync(outPixelAddr - U(1)) && validBufferThree.readAsync(outPixelAddr - U(1))) {
                    when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(outPixelAddr - U(1))) {
                      mainDiagDiff    := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(outPixelAddr - U(1))
                    }.otherwise {
                      mainDiagDiff    := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }

                  when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                  }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
                  }
                }
              }
            }

            is(U(1)){
              when(validBufferTwo.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
              }.otherwise {
                when(validBufferTwo.readAsync(outPixelAddr - U(1))) {
                  io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr - U(1))
                }.otherwise {
                  horizDiff.setAll()
                  when(validBufferOne.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                    when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                      vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                    }.otherwise {
                      vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                    }
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferThree.readAsync(outPixelAddr - U(1))) {
                    when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(outPixelAddr - U(1))) {
                      mainDiagDiff    := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(outPixelAddr - U(1))
                    }.otherwise {
                      mainDiagDiff    := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }

                  when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                  }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
                  }
                }
              }
            }

            is(U(2)){
              when(validBufferThree.readAsync(outPixelAddr)) {
                io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
              }.otherwise {
                when(validBufferThree.readAsync(outPixelAddr - U(1))) {
                  io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr - U(1))
                }.otherwise {
                  horizDiff.setAll()
                  when(validBufferOne.readAsync(outPixelAddr) && validBufferTwo.readAsync(outPixelAddr)) {
                    when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferTwo.readAsync(outPixelAddr)) {
                      vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr)
                    }.otherwise {
                      vertDiff := lineBufferTwo.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                    }
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferTwo.readAsync(outPixelAddr - U(1))) {
                    when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(outPixelAddr - U(1))) {
                      mainDiagDiff    := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                    }.otherwise {
                      mainDiagDiff    := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }

                  when(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferTwo.readAsync(outPixelAddr)) / U(2)).resized
                  }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(outPixelAddr - U(1))) / U(2)).resized
                  }.otherwise {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(outPixelAddr - U(1))) / U(2)).resized
                  }
                }
              }
            }
          }
        }.otherwise {
          switch(nextRowBuffer){
            is(U(0)){
              when(validBufferOne.readAsync(outPixelAddr)){
                io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
              }.otherwise{
                when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferOne.readAsync(U(1) + outPixelAddr)) {
                  when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferOne.readAsync(U(1) + outPixelAddr)) {
                    horizDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                  }.otherwise {
                    horizDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                  }
                }.otherwise {
                  horizDiff.setAll()
                }

                when(validBufferTwo.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                  when(lineBufferTwo.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                    vertDiff := lineBufferTwo.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                  }.otherwise {
                    vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr)
                  }
                }.otherwise {
                  vertDiff.setAll()
                }

                switch(firstRowBuffer) {
                  is(U(1)) {
                    when(validBufferTwo.readAsync(outPixelAddr - U(1)) && validBufferThree.readAsync(U(1) + outPixelAddr)) {
                      when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                        mainDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      }.otherwise {
                        mainDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      }
                    }.otherwise {
                      mainDiagDiff.setAll()
                    }

                    when(validBufferTwo.readAsync(U(1) + outPixelAddr) && validBufferThree.readAsync(outPixelAddr - U(1))) {
                      when(lineBufferTwo.readAsync(U(1) + outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr - U(1))) {
                        counterDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      }.otherwise {
                        counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      }
                    }.otherwise {
                      counterDiagDiff.setAll()
                    }

                    when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                    }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.otherwise {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
                    }
                  }

                  is(U(2)){
                    when(validBufferThree.readAsync(outPixelAddr - U(1)) && validBufferTwo.readAsync(U(1) + outPixelAddr)) {
                      when(lineBufferThree.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                        mainDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      }.otherwise {
                        mainDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      }
                    }.otherwise {
                      mainDiagDiff.setAll()
                    }

                    when(validBufferThree.readAsync(U(1) + outPixelAddr) && validBufferTwo.readAsync(outPixelAddr - U(1))) {
                      when(lineBufferThree.readAsync(U(1) + outPixelAddr) >= lineBufferTwo.readAsync(outPixelAddr - U(1))) {
                        counterDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      }.otherwise {
                        counterDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      }
                    }.otherwise {
                      counterDiagDiff.setAll()
                    }

                    when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                    }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.otherwise {
                      io.dataOut.payload := ((lineBufferThree.readAsync(U(1) + outPixelAddr) +^ lineBufferTwo.readAsync(outPixelAddr - U(1))) / U(2)).resized
                    }
                  }
                }
              }
            }

            is(U(1)){
              when(validBufferTwo.readAsync(outPixelAddr)){
                io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
              }.otherwise{
                when(validBufferTwo.readAsync(outPixelAddr - U(1)) && validBufferTwo.readAsync(U(1) + outPixelAddr)) {
                  when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                    horizDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                  }.otherwise {
                    horizDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                  }
                }.otherwise {
                  horizDiff.setAll()
                }

                when(validBufferOne.readAsync(outPixelAddr) && validBufferThree.readAsync(outPixelAddr)) {
                  when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr)) {
                    vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferThree.readAsync(outPixelAddr)
                  }.otherwise {
                    vertDiff := lineBufferThree.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                  }
                }.otherwise {
                  vertDiff.setAll()
                }

                switch(firstRowBuffer) {
                  is(U(0)) {
                    when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferThree.readAsync(U(1) + outPixelAddr)) {
                      when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                        mainDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      }.otherwise {
                        mainDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      }
                    }.otherwise {
                      mainDiagDiff.setAll()
                    }

                    when(validBufferOne.readAsync(U(1) + outPixelAddr) && validBufferThree.readAsync(outPixelAddr - U(1))) {
                      when(lineBufferOne.readAsync(U(1) + outPixelAddr) >= lineBufferThree.readAsync(outPixelAddr - U(1))) {
                        counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      }.otherwise {
                        counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      }
                    }.otherwise {
                      counterDiagDiff.setAll()
                    }

                    when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                    }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.otherwise {
                      io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr - U(1))) / U(2)).resized
                    }
                  }

                  is(U(2)){
                    when(validBufferThree.readAsync(outPixelAddr - U(1)) && validBufferOne.readAsync(U(1) + outPixelAddr)) {
                      when(lineBufferThree.readAsync(outPixelAddr - U(1)) >= lineBufferOne.readAsync(U(1) + outPixelAddr)) {
                        mainDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      }.otherwise {
                        mainDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      }
                    }.otherwise {
                      mainDiagDiff.setAll()
                    }

                    when(validBufferThree.readAsync(U(1) + outPixelAddr) && validBufferOne.readAsync(outPixelAddr - U(1))) {
                      when(lineBufferThree.readAsync(U(1) + outPixelAddr) >= lineBufferOne.readAsync(outPixelAddr - U(1))) {
                        counterDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      }.otherwise {
                        counterDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      }
                    }.otherwise {
                      counterDiagDiff.setAll()
                    }

                    when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferThree.readAsync(outPixelAddr)) / U(2)).resized
                    }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.otherwise {
                      io.dataOut.payload := ((lineBufferThree.readAsync(U(1) + outPixelAddr) +^ lineBufferOne.readAsync(outPixelAddr - U(1))) / U(2)).resized
                    }
                  }
                }
              }
            }

            is(U(2)){
              when(validBufferThree.readAsync(outPixelAddr)){
                io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
              }.otherwise{
                when(validBufferThree.readAsync(outPixelAddr - U(1)) && validBufferThree.readAsync(U(1) + outPixelAddr)) {
                  when(lineBufferThree.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                    horizDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                  }.otherwise {
                    horizDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                  }
                }.otherwise {
                  horizDiff.setAll()
                }

                when(validBufferOne.readAsync(outPixelAddr) && validBufferTwo.readAsync(outPixelAddr)) {
                  when(lineBufferOne.readAsync(outPixelAddr) >= lineBufferTwo.readAsync(outPixelAddr)) {
                    vertDiff := lineBufferOne.readAsync(outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr)
                  }.otherwise {
                    vertDiff := lineBufferTwo.readAsync(outPixelAddr) - lineBufferOne.readAsync(outPixelAddr)
                  }
                }.otherwise {
                  vertDiff.setAll()
                }

                switch(firstRowBuffer) {
                  is(U(0)) {
                    when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferTwo.readAsync(U(1) + outPixelAddr)) {
                      when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                        mainDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      }.otherwise {
                        mainDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      }
                    }.otherwise {
                      mainDiagDiff.setAll()
                    }

                    when(validBufferOne.readAsync(U(1) + outPixelAddr) && validBufferTwo.readAsync(outPixelAddr - U(1))) {
                      when(lineBufferOne.readAsync(U(1) + outPixelAddr) >= lineBufferTwo.readAsync(outPixelAddr - U(1))) {
                        counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      }.otherwise {
                        counterDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      }
                    }.otherwise {
                      counterDiagDiff.setAll()
                    }

                    when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferTwo.readAsync(outPixelAddr)) / U(2)).resized
                    }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.otherwise {
                      io.dataOut.payload := ((lineBufferOne.readAsync(U(1) + outPixelAddr) +^ lineBufferTwo.readAsync(outPixelAddr - U(1))) / U(2)).resized
                    }
                  }

                  is(U(1)){
                    when(validBufferTwo.readAsync(outPixelAddr - U(1)) && validBufferOne.readAsync(U(1) + outPixelAddr)) {
                      when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferOne.readAsync(U(1) + outPixelAddr)) {
                        mainDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      }.otherwise {
                        mainDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      }
                    }.otherwise {
                      mainDiagDiff.setAll()
                    }

                    when(validBufferTwo.readAsync(U(1) + outPixelAddr) && validBufferOne.readAsync(outPixelAddr - U(1))) {
                      when(lineBufferTwo.readAsync(U(1) + outPixelAddr) >= lineBufferOne.readAsync(outPixelAddr - U(1))) {
                        counterDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      }.otherwise {
                        counterDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      }
                    }.otherwise {
                      counterDiagDiff.setAll()
                    }

                    when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr) +^ lineBufferTwo.readAsync(outPixelAddr)) / U(2)).resized
                    }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                    }.otherwise {
                      io.dataOut.payload := ((lineBufferTwo.readAsync(U(1) + outPixelAddr) +^ lineBufferOne.readAsync(outPixelAddr - U(1))) / U(2)).resized
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }.otherwise {
    when(bufferRowCount === U(4) * bmpHeight || bufferWAddr > U(1) + outPixelAddr) {
      io.dataOut.valid := True
      switch(nextRowBuffer) {
        is(U(0)) {
          when(outPixelAddr === U(0)) {
            when(validBufferOne.readAsync(outPixelAddr)) {
              io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
            }.otherwise {
              when(validBufferOne.readAsync(U(1) + outPixelAddr)) {
                io.dataOut.payload := lineBufferOne.readAsync(U(1) + outPixelAddr)
              }.otherwise {
                when(firstRowBuffer === U(1)) {
                  when(validBufferTwo.readAsync(outPixelAddr)) {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferTwo.readAsync(U(1) + outPixelAddr)
                  }
                }.otherwise {
                  when(validBufferThree.readAsync(outPixelAddr)) {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferThree.readAsync(U(1) + outPixelAddr)
                  }
                }
              }
            }
          }.elsewhen(outPixelAddr === U(4) * bmpWidth - U(1)) {
            when(validBufferOne.readAsync(outPixelAddr)) {
              io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
            }.otherwise {
              when(validBufferOne.readAsync(outPixelAddr - U(1))) {
                io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr - U(1))
              }.otherwise {
                when(firstRowBuffer === U(1)) {
                  when(validBufferTwo.readAsync(outPixelAddr)) {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr - U(1))
                  }
                }.otherwise {
                  when(validBufferThree.readAsync(outPixelAddr)) {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr - U(1))
                  }
                }
              }
            }
          }.otherwise {
            when(validBufferOne.readAsync(outPixelAddr)){
              io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
            }.otherwise{
              when(validBufferOne.readAsync(outPixelAddr - U(1)) && validBufferOne.readAsync(U(1) + outPixelAddr)) {
                when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferOne.readAsync(U(1) + outPixelAddr)) {
                  horizDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                }.otherwise {
                  horizDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                }
              }.otherwise {
                horizDiff.setAll()
              }

              switch(firstRowBuffer) {
                is(U(1)) {
                  when(validBufferTwo.readAsync(outPixelAddr)) {
                    vertDiff.clearAll()
                  }.otherwise {
                    vertDiff.setAll()
                  }
                  when(validBufferTwo.readAsync(outPixelAddr - U(1)) || validBufferTwo.readAsync(U(1) + outPixelAddr)) {
                    when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }
                }
                is(U(2)) {
                  when(validBufferThree.readAsync(outPixelAddr)) {
                    vertDiff.clearAll()
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferThree.readAsync(outPixelAddr - U(1)) || validBufferThree.readAsync(U(1) + outPixelAddr)) {
                    when(lineBufferThree.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }
                }
              }
              when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
              }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                switch(firstRowBuffer) {
                  is(U(1)) {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
                  }
                  is(U(2)) {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
                  }
                }
              }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                switch(firstRowBuffer) {
                  is(U(1)) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                  is(U(2)) {
                    io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }.otherwise {
                switch(firstRowBuffer) {
                  is(U(1)) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                  is(U(2)) {
                    io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }
            }

          }
        }

        is(U(1)) {
          when(outPixelAddr === U(0)) {
            when(validBufferTwo(outPixelAddr)) {
              io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
            }.otherwise {
              when(validBufferTwo(U(1) + outPixelAddr)) {
                io.dataOut.payload := lineBufferTwo.readAsync(U(1) + outPixelAddr)
              }.otherwise {
                when(firstRowBuffer === U(0)) {
                  when(validBufferOne(outPixelAddr)) {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferOne.readAsync(U(1) + outPixelAddr)
                  }
                }.otherwise {
                  when(validBufferThree(outPixelAddr)) {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferThree.readAsync(U(1) + outPixelAddr)
                  }
                }
              }
            }
          }.elsewhen(outPixelAddr === U(4) * bmpWidth - U(1)) {
            when(validBufferTwo.readAsync(outPixelAddr)) {
              io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
            }.otherwise {
              when(validBufferTwo.readAsync(outPixelAddr - U(1))) {
                io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr - U(1))
              }.otherwise {
                when(firstRowBuffer === U(0)) {
                  when(validBufferOne.readAsync(outPixelAddr)) {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr - U(1))
                  }
                }.otherwise {
                  when(validBufferThree.readAsync(outPixelAddr)) {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr - U(1))
                  }
                }
              }
            }
          }.otherwise {
            when(validBufferTwo.readAsync(outPixelAddr)){
              io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
            }.otherwise{
              when(validBufferTwo.readAsync(outPixelAddr - U(1)) && validBufferTwo.readAsync(U(1) + outPixelAddr)) {
                when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                  horizDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                }.otherwise {
                  horizDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                }
              }.otherwise {
                horizDiff.setAll()
              }

              switch(firstRowBuffer) {
                is(U(0)) {
                  when(validBufferOne.readAsync(outPixelAddr)) {
                    vertDiff.clearAll()
                  }.otherwise {
                    vertDiff.setAll()
                  }
                  when(validBufferOne.readAsync(outPixelAddr - U(1)) || validBufferOne.readAsync(U(1) + outPixelAddr)) {
                    when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferOne.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }
                }
                is(U(2)) {
                  when(validBufferThree.readAsync(outPixelAddr)) {
                    vertDiff.clearAll()
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferThree.readAsync(outPixelAddr - U(1)) || validBufferThree.readAsync(U(1) + outPixelAddr)) {
                    when(lineBufferThree.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }
                }
              }
              when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
              }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                switch(firstRowBuffer) {
                  is(U(0)) {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
                  }
                  is(U(2)) {
                    io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
                  }
                }
              }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                switch(firstRowBuffer) {
                  is(U(0)) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                  is(U(2)) {
                    io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }.otherwise {
                switch(firstRowBuffer) {
                  is(U(0)) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                  is(U(2)) {
                    io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }
            }
          }
        }

        is(U(2)) {
          when(outPixelAddr === U(0)) {
            when(validBufferThree(outPixelAddr) === True) {
              io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
            }.otherwise {
              when(validBufferThree(U(1) + outPixelAddr) === True) {
                io.dataOut.payload := lineBufferThree.readAsync(U(1) + outPixelAddr)
              }.otherwise {
                when(firstRowBuffer === U(0)) {
                  when(validBufferOne(outPixelAddr) === True) {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferOne.readAsync(U(1) + outPixelAddr)
                  }
                }.otherwise {
                  when(validBufferTwo(outPixelAddr) === True) {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferTwo.readAsync(U(1) + outPixelAddr)
                  }
                }
              }
            }
          }.elsewhen(outPixelAddr === U(4) * bmpWidth - U(1)) {
            when(validBufferThree.readAsync(outPixelAddr) === True) {
              io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
            }.otherwise {
              when(validBufferThree.readAsync(outPixelAddr - U(1)) === True) {
                io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr - U(1))
              }.otherwise {
                when(firstRowBuffer === U(0)) {
                  when(validBufferOne.readAsync(outPixelAddr) === True) {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr - U(1))
                  }
                }.otherwise {
                  when(validBufferTwo.readAsync(outPixelAddr) === True) {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
                  }.otherwise {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr - U(1))
                  }
                }
              }
            }
          }.otherwise {
            when(validBufferThree.readAsync(outPixelAddr)){
              io.dataOut.payload := lineBufferThree.readAsync(outPixelAddr)
            }.otherwise{
              when(validBufferThree.readAsync(outPixelAddr - U(1)) === True && validBufferThree.readAsync(U(1) + outPixelAddr) === True) {
                when(lineBufferThree.readAsync(outPixelAddr - U(1)) >= lineBufferThree.readAsync(U(1) + outPixelAddr)) {
                  horizDiff := lineBufferThree.readAsync(outPixelAddr - U(1)) - lineBufferThree.readAsync(U(1) + outPixelAddr)
                }.otherwise {
                  horizDiff := lineBufferThree.readAsync(U(1) + outPixelAddr) - lineBufferThree.readAsync(outPixelAddr - U(1))
                }
              }.otherwise {
                horizDiff.setAll()
              }

              switch(firstRowBuffer) {
                is(U(0)) {
                  when(validBufferOne.readAsync(outPixelAddr) === True) {
                    vertDiff.clearAll()
                  }.otherwise {
                    vertDiff.setAll()
                  }
                  when(validBufferOne.readAsync(outPixelAddr - U(1)) === True || validBufferOne.readAsync(U(1) + outPixelAddr) === True) {
                    when(lineBufferOne.readAsync(outPixelAddr - U(1)) >= lineBufferOne.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferOne.readAsync(outPixelAddr - U(1)) - lineBufferOne.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferOne.readAsync(U(1) + outPixelAddr) - lineBufferOne.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }
                }
                is(U(1)) {
                  when(validBufferTwo.readAsync(outPixelAddr) === True) {
                    vertDiff.clearAll()
                  }.otherwise {
                    vertDiff.setAll()
                  }

                  when(validBufferTwo.readAsync(outPixelAddr - U(1)) === True || validBufferTwo.readAsync(U(1) + outPixelAddr) === True) {
                    when(lineBufferTwo.readAsync(outPixelAddr - U(1)) >= lineBufferTwo.readAsync(U(1) + outPixelAddr)) {
                      mainDiagDiff    := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                      counterDiagDiff := lineBufferTwo.readAsync(outPixelAddr - U(1)) - lineBufferTwo.readAsync(U(1) + outPixelAddr)
                    }.otherwise {
                      mainDiagDiff    := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                      counterDiagDiff := lineBufferTwo.readAsync(U(1) + outPixelAddr) - lineBufferTwo.readAsync(outPixelAddr - U(1))
                    }
                  }.otherwise {
                    mainDiagDiff.setAll()
                    counterDiagDiff.setAll()
                  }
                }
              }
              when(horizDiff <= vertDiff && horizDiff <= mainDiagDiff && horizDiff <= counterDiagDiff) {
                io.dataOut.payload := ((lineBufferThree.readAsync(outPixelAddr - U(1)) +^ lineBufferThree.readAsync(U(1) + outPixelAddr)) / U(2)).resized
              }.elsewhen(vertDiff <= horizDiff && vertDiff <= mainDiagDiff && vertDiff <= counterDiagDiff) {
                switch(firstRowBuffer) {
                  is(U(0)) {
                    io.dataOut.payload := lineBufferOne.readAsync(outPixelAddr)
                  }
                  is(U(1)) {
                    io.dataOut.payload := lineBufferTwo.readAsync(outPixelAddr)
                  }
                }
              }.elsewhen(mainDiagDiff <= horizDiff && mainDiagDiff <= vertDiff && mainDiagDiff <= counterDiagDiff) {
                switch(firstRowBuffer) {
                  is(U(0)) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                  is(U(1)) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
              }.otherwise {
                switch(firstRowBuffer) {
                  is(U(0)) {
                    io.dataOut.payload := ((lineBufferOne.readAsync(outPixelAddr - U(1)) +^ lineBufferOne.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                  is(U(1)) {
                    io.dataOut.payload := ((lineBufferTwo.readAsync(outPixelAddr - U(1)) +^ lineBufferTwo.readAsync(U(1) + outPixelAddr)) / U(2)).resized
                  }
                }
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
//        sameBuffer := False
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

  when(frameStart && io.dataOut.valid) {
    io.frameStartOut := True
  }
  when(frameStart && io.dataOut.fire) {
    frameStart := False
  }

  io.inpThreeCompleteOut.allowOverride
  io.startOut.allowOverride
  io.inpValidOut.allowOverride
  io.inpValidOut         := True
  io.inpThreeCompleteOut := interComplete
  io.startOut            := slaveStart
}

object gen extends App{
  SpinalVerilog(InterpolationStep3(IPConfig(5, 5))).printPruned()
}
