

package mylib

import spinal.core.{UInt, _}
import spinal.lib.fsm._



//Hardware definition
class obuff_rd_ctrl(pox:Int, poy: Int, pic: Int, poc: Int, max_ow: Int, max_ic: Int, max_oc: Int, max_ksize: Int) extends Component {
  val io = new Bundle{
    val clk = in Bool
    val rstn = in Bool
    val ppen = in Bool
    val kw = in UInt(log2Up(max_ksize) bits)
    val tic = in UInt(log2Up(max_ic) bits)
    val toc = in UInt(log2Up(max_oc) bits)
    val tox = in UInt(log2Up(max_ow) bits)
    val toy = in UInt(log2Up(max_ow) bits)
    val bias_rden = out Bool
    val obuff_rden = out Bool
    val counter = out UInt(log2Up(max_ic) bits)
  }

  val myCD = new ClockDomain(
    clock = io.clk,
    reset = io.rstn,
    config = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = ASYNC,
      resetActiveLevel = LOW
    )
  )

  val mainArea = myCD {

    val bias_rden_ff1 = Reg(Bool) init (False)
    val obuff_rden_ff1 = Reg(Bool) init (False)
    val bias_rden_ff2 = Reg(Bool) init (False)
    val obuff_rden_ff2 = Reg(Bool) init (False)

    val IS_fsm = new StateMachine {
      //state declaration
      val IDLE = new State with EntryPoint
      val CALC = new State
      val OC_CHANGE = new State
      val IC_CHANGE = new State
      val OX_SHIFT = new State
      val OY_SHIFT = new State

      //counter declaration
      val kernel_cnt = Reg(UInt(log2Up(max_ksize * max_ksize) bits)) init(0)
      val tic_cnt = Reg(UInt(log2Up(max_ic) bits)) init(0)
      val toc_cnt = Reg(UInt(log2Up(max_oc) bits)) init(0)
      val tox_cnt = Reg(UInt(log2Up(max_ow) bits)) init(0)
      val toy_cnt = Reg(UInt(log2Up(max_ow) bits)) init(0)
      kernel_cnt.addAttribute("Mark_DEBUG")

      val kernel_done = (kernel_cnt === (io.kw * io.kw - 1))
      val tic_done = Bool
      val toc_done = Bool
      val tox_done = Bool
      val toy_done = Bool
      when((tic_cnt + pic) >= io.tic) {
        tic_done := True
      } otherwise (tic_done := False)
      toc_done := ((toc_cnt + poc) >= (io.toc))
      tox_done := ((tox_cnt + pox) >= (io.tox))
      toy_done := ((toy_cnt + poy) >= (io.toy))
      io.counter := tic_cnt

      //state declaration
      IDLE
        .whenIsActive {
          when(io.ppen) {
            goto(CALC)
          }
        }

      CALC
        .whenIsActive {
          when(io.ppen) {
            when(kernel_done) {
              when(toc_done) {
                when(tic_done) {
                  when(tox_done) {
                    when(toy_done) {
                      goto(IDLE)
                    } otherwise (goto(OY_SHIFT))
                  } otherwise (goto(OX_SHIFT))
                } otherwise (goto(IC_CHANGE))
              } otherwise (goto(OC_CHANGE))
            } otherwise {
              kernel_cnt := kernel_cnt + 1
            }
          }
        }

      OC_CHANGE
        .whenIsActive {
          when(io.ppen) {
            kernel_cnt := 0
            toc_cnt := toc_cnt + poc
            goto(CALC)
          }
        }

      IC_CHANGE
        .onEntry {
          when(tic_done && io.ppen) {
            bias_rden_ff1 := True
            obuff_rden_ff1 := True
          } elsewhen (tic_cnt =/= 0) {
            obuff_rden_ff1 := True
          } otherwise {
            bias_rden_ff1 := False
            obuff_rden_ff1 := False
          }
        }
        .whenIsActive {
          when(io.ppen) {
            kernel_cnt := 0
            toc_cnt := 0
            tic_cnt := tic_cnt + pic
            goto(CALC)
          }

        }

      OX_SHIFT
        .whenIsActive {
          when(io.ppen) {
            kernel_cnt := 0
            toc_cnt := 0
            tic_cnt := 0
            tox_cnt := tox_cnt + pox
            goto(CALC)
          }
        }

      OY_SHIFT
        .whenIsActive {
          when(io.ppen) {
            kernel_cnt := 0
            toc_cnt := 0
            tic_cnt := 0
            tox_cnt := 0
            toy_cnt := toy_cnt + poy
            goto(CALC)
          }
        }

    }

    bias_rden_ff2 := bias_rden_ff1
    obuff_rden_ff2 := obuff_rden_ff1
    io.bias_rden := bias_rden_ff1 && (!bias_rden_ff2)
    io.obuff_rden := obuff_rden_ff1 && (!obuff_rden_ff2)


  }
}


//Generate the MyTopLevel's Verilog
object obuff_rd_ctrl {
  def main(args: Array[String]) {
    SpinalVerilog(new obuff_rd_ctrl(4,4,4,4,256,256,256,7))
  }
}

/*
//Generate the MyTopLevel's VHDL
object MyTopLevelVhdl {
  def main(args: Array[String]) {
    SpinalVhdl(new MyTopLevel)
  }
}


//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

//Generate the MyTopLevel's Verilog using the above custom configuration.
object MyTopLevelVerilogWithCustomConfig {
  def main(args: Array[String]) {
    MySpinalConfig.generateVerilog(new MyTopLevel)
  }
}

 */

import spinal.core.sim._

//MyTopLevel's testbench
object obuff_rd_ctrlSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new obuff_rd_ctrl(4,4,4,4,256,256,256,7)){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.myCD.forkStimulus(10)
      /*fork{
        dut.myCD.assertReset()
        dut.myCD.fallingEdge()
        sleep(10)
        while(true){
          dut.myCD.clockToggle()
          sleep(10)
        }
      }*/
      //var modelState = 0
      for(idx <- 0 to 9999){
        //Drive the dut inputs with random values
        dut.io.ppen #= true
        dut.io.kw #= 3
        dut.io.tic #= 128
        dut.io.toc #= 128
        dut.io.tox #= 128
        dut.io.toy #= 128

        //Wait a rising edge on the clock
        dut.myCD.waitRisingEdge()

        //Check that the dut values match with the reference model ones
        /*val modelFlag = modelState == 0 || dut.io.cond1.toBoolean
        assert(dut.io.state.toInt == modelState)
        assert(dut.io.flag.toBoolean == modelFlag)*/

        //Update the reference model value
        /*if(dut.io.cond0.toBoolean) {
          modelState = (modelState + 1) & 0xFF
        }*/
      }
    }
  }
}
