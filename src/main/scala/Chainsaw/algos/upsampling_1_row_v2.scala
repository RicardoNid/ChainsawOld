package Chainsaw.algos

import spinal.lib._
import spinal.core._
import spinal.core.sim._
import scala.collection.mutable.ListBuffer

case class upsampling_1_row_config (threshold: Int, IMG_width:Int = 960)

case class upsampling_1_row_v2(cofig:upsampling_1_row_config) extends Component{
  val io = new Bundle{
    val data_in  = slave(Stream(UInt(8 bits)))
    val data_out = master(Stream(UInt(8 bits)))
    val in_frame_flag = in(Bool)
    val in_row_flag = in(Bool)
    val out_frame_flag = out(Bool)
    val out_row_flag = out(Bool)
    val interpolated = out(Bool)
  }
  val previous_pixel = Reg(UInt(8 bits)) init(U(0))
  val current_pixel  = Reg(UInt(8 bits)) init(U(0))
  val current_frame  = Reg(Bool) init(false)
  val current_row    = Reg(Bool) init(false)

  val diff_btw_pixels = UInt(8 bits)
  val sum_btw_pixels = UInt(9 bits)
  val value_for_itpl  = UInt(8 bits)

  val work_state = Reg(Bool()) init(false)simPublic()       // 1: busy 0:free

  val counter = Counter(0,cofig.IMG_width)
  val transmission_counter = Counter(1,2)

  io.data_in.ready := !work_state
  when(io.data_in.valid & io.data_in.ready)
  {
    work_state :=  True
    current_pixel := io.data_in.payload
    current_frame := io.in_frame_flag
    current_row   := io.in_row_flag
    counter.increment()
    when(counter.willOverflow){
      counter := U(1)
    }
  }
  when(work_state){    // successfully receive the data_in
    when(counter === 1){ // mean the start of the row
      diff_btw_pixels := 0
      sum_btw_pixels  := previous_pixel +^ current_pixel  // the result is no used
      value_for_itpl  := current_pixel
      io.interpolated := True
    }otherwise{
      when(current_pixel > previous_pixel){
        diff_btw_pixels := current_pixel - previous_pixel
      }otherwise{
        diff_btw_pixels := previous_pixel - current_pixel
      }
      when(diff_btw_pixels < cofig.threshold){
        sum_btw_pixels := current_pixel +^ previous_pixel
        value_for_itpl := sum_btw_pixels >> 1
        io.interpolated:= True
      }otherwise{
        sum_btw_pixels := 0
        value_for_itpl := 0
        io.interpolated:= False
      }
    }

    io.data_out.valid   := True
    //io.out_frame_flag   := current_frame
    //io.out_row_flag     := current_row
    when(io.data_out.ready){
      transmission_counter.increment()
    }
    when(transmission_counter === 1){
      io.data_out.payload := value_for_itpl
      io.out_frame_flag   := current_frame   //5.13
      io.out_row_flag     := False           //5.13
    }otherwise {
      io.data_out.payload := current_pixel
      io.interpolated := True              //5.13
      io.out_frame_flag   := False        //5.13
      io.out_row_flag     := current_row           //5.13
    }
  }otherwise{
    io.data_out.valid := False

    io.data_out.payload := 0
    io.out_row_flag     := False
    io.out_frame_flag   := False
    io.interpolated     := False

    diff_btw_pixels     := 0
    sum_btw_pixels      := 0
    value_for_itpl      := 0

  }
  when((io.data_out.ready) && (io.data_out.valid) && (transmission_counter === 2)){
    work_state := False
    previous_pixel := current_pixel
  }

}


import scala.math._
object upsample_1_row extends App{
  //SpinalVerilog(upsampling_1_row_v2(upsampling_1_row_config(150)))

  def interpolation_result(data_in : ListBuffer[Int], IMG_width:Int, IMG_height:Int, threshold: Int): ListBuffer[Int]={
    val IMG_size = IMG_width * IMG_height
    val data_interpolated = ListBuffer.fill(IMG_size * 2)(-1)
    var diff:Int = 0
    for(i <- 0 until IMG_size){
      if (i%IMG_width==0){
        data_interpolated(2 * i) = data_in(i)
        data_interpolated(2 * i + 1) = data_in(i)
      }else{
        diff = abs(data_in(i) - data_in(i-1))
        if (diff < threshold){
          data_interpolated(2 * i) = (data_in(i) + data_in(i-1))/2
        }else{
          data_interpolated(2 * i) = 0
        }
        data_interpolated(2 * i + 1) = data_in(i)
      }
    }
    data_interpolated
  }

  val compiled = SimConfig.withFstWave.allOptimisation.compile(upsampling_1_row_v2(upsampling_1_row_config(150,6)))

  //generate random input data, for verify, assume input image array size is 6 x 6
  val data_in = ListBuffer(0)
  val frame   = ListBuffer(true)
  val row     = ListBuffer(true)

  // set fix input data
  //  val data_in  = ListBuffer(135,246,128,137,89, 76,
  //                       160,10 ,23, 200,100,120,
  //                       110,120,130,137,180, 20,
  //                       180,20, 130,210,87, 96,
  //                       28, 54, 210,180,134,98,
  //                       200,250,37, 27, 50, 45)  //6 * 6  --> 12 * 6
  //  val frame_flag_in = List(true,false,false,false,false,false,
  //    false,false,false,false,false,false,
  //    false,false,false,false,false,false,
  //    false,false,false,false,false,false,
  //    false,false,false,false,false,false,
  //    false,false,false,false,false,false)
  //  val row_flag_in = List(true,false,false,false,false,false,
  //    true,false,false,false,false,false,
  //    true,false,false,false,false,false,
  //    true,false,false,false,false,false,
  //    true,false,false,false,false,false,
  //    true,false,false,false,false,false)
  //
  //  val valid_in = true
  //  val data_out_ready = true

  var data_out = ListBuffer.fill(72)(-1)
  var frame_flag_out = ListBuffer.fill(72)(false)
  var row_flag_out  = ListBuffer.fill(72)(false)
  var interpolated_flag = ListBuffer.fill(72)(false)

  var idy,idf = -1
  var idx = 0
  compiled.doSim{dut =>
    dut.clockDomain.forkStimulus(10)
    val fork_in = fork{
      while (true){
        dut.clockDomain.waitSampling()
        //        //fix data input
        //        dut.io.data_in.valid.randomize()
        //        if(dut.io.data_in.valid.toBoolean){
        //          if(dut.io.data_in.ready.toBoolean){
        //            idx = idx + 1
        //            if(idx == 36){idx = 35}
        //          }
        //        }
        //        dut.io.data_in.payload #= data_in(idx)
        //        dut.io.in_frame_flag   #= frame_flag_in(idx)
        //        dut.io.in_row_flag     #= row_flag_in(idx)
        //        //dut.io.data_out.ready  #= data_out_ready
        //        dut.io.data_out.ready.randomize()

        //random data input
        dut.io.data_in.valid.randomize()
        dut.io.data_out.ready.randomize()
        dut.io.data_in.payload.randomize()
        dut.io.in_row_flag.randomize()
        dut.io.in_frame_flag.randomize()
        if(dut.io.data_in.valid.toBoolean && dut.io.data_in.ready.toBoolean){
          data_in.append(dut.io.data_in.payload.toInt)
          frame.append(dut.io.in_frame_flag.toBoolean)
          row.append(dut.io.in_row_flag.toBoolean)
        }


      }
    }
    val fork_out = fork{
      while(true){
        dut.clockDomain.waitSampling()
        if(dut.io.data_out.valid.toBoolean & dut.io.data_out.ready.toBoolean){
          idy = idy + 1
          if(idy == 72){
            println()
            println(s"the data_in")
            for (i<-1 until 37 by 6){
              println(s"${data_in(i)} ${data_in(i+1)} ${data_in(i+2)} ${data_in(i+3)} ${data_in(i+4)} ${data_in(i+5)}")
            }
            println()
            // val result_test = interpolation_result(data_in)
            val result_test = interpolation_result(data_in.tail,6,6,dut.cofig.threshold)
            println("the data result computed by interpolation_result function ")
            for (i<-0 until 72 by 12){
              println(s"${result_test(i)} ${result_test(i+1)} ${result_test(i+2)} ${result_test(i+3)} ${result_test(i+4)} ${result_test(i+5)}" + " " +
                s"${result_test(i+6)} ${result_test(i+7)} ${result_test(i+8)} ${result_test(i+9)} ${result_test(i+10)} ${result_test(i+11)}")
            }
            println()
            println("the data result computed by RTL")
            for (i<-0 until 72 by 12){
              println(s"${data_out(i)} ${data_out(i+1)} ${data_out(i+2)} ${data_out(i+3)} ${data_out(i+4)} ${data_out(i+5)}" + " " +
                s"${data_out(i+6)} ${data_out(i+7)} ${data_out(i+8)} ${data_out(i+9)} ${data_out(i+10)} ${data_out(i+11)}")
            }
            println()
            println("the interpolation flag computed by RTL")
            for(i<-0 until 72 by 12){
              println(s"${interpolated_flag(i)} ${interpolated_flag(i+1)} ${interpolated_flag(i+2)} ${interpolated_flag(i+3)} ${interpolated_flag(i+4)} ${interpolated_flag(i+5)}" +
                s"${interpolated_flag(i+6)} ${interpolated_flag(i+7)} ${interpolated_flag(i+8)} ${interpolated_flag(i+9)} ${interpolated_flag(i+10)} ${interpolated_flag(i+11)}")
            }
            println()
            println("the frame_start flag")
            for(i<-1 until 37 by 6){
              println(s"${frame(i)} ${"false"} ${frame(i+1)} ${"false"} ${frame(i+2)} ${"false"} ${frame(i+3)} ${"false"} ${frame(i+4)} ${"false"} ${frame(i+5)} ${"false"} ")
            }
            println()
            println("the frame_start flag computed by RTL")
            for(i<-0 until 72 by 12){
              println(s"${frame_flag_out(i)} ${frame_flag_out(i+1)} ${frame_flag_out(i+2)} ${frame_flag_out(i+3)} ${frame_flag_out(i+4)} ${frame_flag_out(i+5)}" +
                s"${frame_flag_out(i+6)} ${frame_flag_out(i+7)} ${frame_flag_out(i+8)} ${frame_flag_out(i+9)} ${frame_flag_out(i+10)} ${frame_flag_out(i+11)}")
            }
            println()
            println("the row_end flag ")
            for(i<-1 until 37 by 6){
              println(s"${"false"} ${row(i)} ${"false"} ${row(i+1)} ${"false"} ${row(i+2)} ${"false"} ${row(i+3)} ${"false"} ${row(i+4)} ${"false"} ${row(i+5)}")
            }
            println()
            println("the row_end flag computed by RTL")
            for(i<-0 until 72 by 12){
              println(s"${row_flag_out(i)} ${row_flag_out(i+1)} ${row_flag_out(i+2)} ${row_flag_out(i+3)} ${row_flag_out(i+4)} ${row_flag_out(i+5)}" +
                s"${row_flag_out(i+6)} ${row_flag_out(i+7)} ${row_flag_out(i+8)} ${row_flag_out(i+9)} ${row_flag_out(i+10)} ${row_flag_out(i+11)}")
            }
            println()
            println(s"start to verify the correctness:")
            println(s"pixel_result:${result_test == data_out}")
            val frame_verify = ListBuffer(true)
            for (elem <- frame.tail.init){
              frame_verify.append(elem)
              frame_verify.append(false)
            }
            val row_verify = ListBuffer(true)
            for (elem <- row.tail.init){
              row_verify.append(false)
              row_verify.append(elem)
            }

            println(s"frame_result:${frame_verify.tail == frame_flag_out}")
            println(s"row_result:${row_verify.tail == row_flag_out}")

            simSuccess()
          }
          data_out(idy) = dut.io.data_out.payload.toInt
          frame_flag_out(idy) = dut.io.out_frame_flag.toBoolean
          row_flag_out(idy) = dut.io.out_row_flag.toBoolean
          interpolated_flag(idy) = dut.io.interpolated.toBoolean
        }

      }
    }
    fork_in.join()
    fork_out.join()

  }

}