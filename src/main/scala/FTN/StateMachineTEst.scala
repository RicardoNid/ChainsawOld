//package FTN
//
//
//import spinal.core._
//import spinal.core.sim.{SimBitVectorPimper, SimClockDomainPimper, SimConfig, sleep}
//import spinal.lib.fsm._
//
//import scala.util.Random
//
//class StateMachine_test extends Component {
//
//  ///define io
//  val incode = in Bits (2 bits)
//  val decoder = out Bits (6 bits)
//
//  /////def function
//
//  def min2(a: UInt, b: UInt) = {
//    Mux(a <= b, Vec(a,U(0)), Vec(b,U(1)))
//  }
//
//  def minpath(BM: Vec[UInt]) = {
//    //default : BM has 4 elements
//    val tempmin1 = min2(BM(0), BM(1))
//    val tempmin2 = min2(BM(2), BM(3))
//    val tempmin3 = min2(tempmin1(0), tempmin2(0))
//    val out = UInt(2 bits)
//    when(tempmin3(1) === 0) {
//      when(tempmin1(1) === 0) {
//        out := 0
//      }.otherwise(out := 1)
//    }.otherwise {
//      when(tempmin2(1) === 0) {
//        out := 2
//      }.otherwise(out := 3)
//    }
//    out
//
//
//  }
//
//  def decode2(a: UInt, b: UInt) = {
//    val out = Bool
//    when(a === b) {
//      when(a === 0) {
//        out := False
//      }.otherwise(out := True)
//    }.elsewhen(a > b) {
//      out := False
//    }.otherwise(out := True)
//    out
//
//  }
//
//  //
//
//  val BM = Reg(Vec(UInt(4 bits), 4))
//  val PreBM = Reg(Vec(UInt(4 bits), 4))
//  val PreState = Reg(Vec(UInt(2 bits), 4 * 6))
//
//
//  ////initial////////////////////////////////////////////////////////////////////////////////////////////////////////
//  PreBM(0).init(0);
//  PreBM(1).init(6);
//  PreBM(2).init(6);
//  PreBM(3).init(6);
//
//  val source_state = Reg(Vec(UInt(2 bits), 4 * 2)).allowUnsetRegToAvoidLatch
//  source_state(0).init(0);
//  source_state(1).init(1)
//  source_state(2).init(2);
//  source_state(3).init(3)
//  source_state(4).init(0);
//  source_state(5).init(1)
//  source_state(6).init(2);
//  source_state(7).init(3)
//
//  val PM = Reg(Vec(UInt(4 bits), 4 * 8)).allowUnsetRegToAvoidLatch
//  PM(0).init(0);
//  PM(1).init(2);
//  PM(2).init(1);
//  PM(3).init(1);
//  PM(4).init(1);
//  PM(5).init(1);
//  PM(6).init(2);
//  PM(7).init(0);
//  PM(8).init(1);
//  PM(9).init(1);
//  PM(10).init(2);
//  PM(11).init(0);
//  PM(12).init(0);
//  PM(13).init(2);
//  PM(14).init(1);
//  PM(15).init(1);
//  PM(16).init(2);
//  PM(17).init(0);
//  PM(18).init(1);
//  PM(19).init(1);
//  PM(20).init(1);
//  PM(21).init(1);
//  PM(22).init(0);
//  PM(23).init(2);
//  PM(24).init(1);
//  PM(25).init(1);
//  PM(26).init(0);
//  PM(27).init(2);
//  PM(28).init(2);
//  PM(29).init(0);
//  PM(30).init(1);
//  PM(31).init(1);
//  //////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//
//  val fsm = new StateMachine {
//
//    val IDLE = new State with EntryPoint
//    val CALBM = new State
//    val TEMP = new State
//    val BACK = new State
//    val END = new State
//
//
//    val counter = Reg(UInt(5 bits)) init (0)
//    val counterback = Reg(UInt(3 bits)) init (1)
//    val back = Reg(Vec(UInt(2 bits), 7))
//
//
//    decoder := 0
//
//    IDLE
//      .whenIsActive {
//        BM(0) := PreBM(0)
//        BM(1) := PreBM(1)
//        BM(2) := PreBM(2)
//        BM(3) := PreBM(3)
//        goto(CALBM)
//      }
//
//    CALBM
//
//      .whenIsActive {
//
//        (0 until 4).foreach { i => ////4->StateNum
//
//          val minresult = min2(BM(source_state(i * 2)) + PM(i * 8 + (B"0" ## incode).asUInt * 2), BM(source_state(i * 2 + 1)) + PM(i * 8 + (B"0" ## incode).asUInt * 2 + 1))
//          //calculate BM
//          BM(i) := minresult(0)
//          //calculate PreState
//          switch(minresult(1)) {
//            is(0) {
//              PreState(i * 6 + counter) := source_state(i * 2)
//            }
//            is(1) {
//              PreState(i * 6 + counter) := source_state(i * 2 + 1)
//            }
//          }
//
//
//        } //end foreach
//
//        counter := counter + 1
//        when(counter === 5) {
//          counter := 0
//          goto(TEMP)
//
//        }
//      } //end CALBM
//
//
//    TEMP
//      .whenIsActive {
//        back(6) := minpath(BM)
//        goto(BACK)
//      }
//
//    BACK
//      .onEntry(counterback := 1)
//      .whenIsActive {
//        //        //sub fsm
//        //        def  subfsm=new StateMachine{
//        //          val ready=new State with EntryPoint
//        //          val backs =new State
//        //          val backend =new State
//        //
//        //          //val counterback=Reg(UInt(3 bits)) init(0)
//        //
//        //          ready
//        //            .whenIsActive(goto(backs))
//        //
//        //          backs
//        //            .onEntry( counterback:=1)
//        //            .whenIsActive{
//        //              back(6-counterback):=PreState(back(7-counterback)*6+6-counterback)
//        //              //counterback
//        //              counterback:=counterback+1
//        //              when(counterback===6){
//        //                counterback:=0
//        //                goto(backend)
//        //              }
//        //            }
//        //
//        //          backend
//        //            .exit()
//        //
//        //
//        //        }
//        //        subfsm.exitFsm()
//        // goto(END)
//        //back
//        back(6 - counterback) := PreState(back(7 - counterback) * 6 + 6 - counterback)
//        //counterback
//        counterback := counterback + 1
//        when(counterback === 6) {
//          counterback := 0
//          goto(END)
//        }
//
//      } //end back
//
//    END
//      .whenIsActive {
//        //decoder
//        (0 until 6).foreach { i =>
//          decoder(i) := (decode2(back(i), back(i + 1)))
//        }
//        PreBM(0) := BM(0)
//        PreBM(1) := BM(1)
//        PreBM(2) := BM(2)
//        PreBM(3) := BM(3)
//
//        goto(IDLE)
//      }
//
//
//  } //end fsm
//
//} //end class
//
//object StateMachine_test {
//  def main(args: Array[String]): Unit = {
//    SpinalVerilog(new StateMachine_test)
//  }
//
//  SimConfig.withWave.compile(new StateMachine_test).doSim { dut =>
//    dut.clockDomain.
//    // dut.clockDomain.deassertReset()
//    var idx = 0
//
//    while (idx < 100) {
//      dut.incode #= Random.nextInt(4)
//      sleep(1) // Sleep 1 simulation timestep
//      idx += 1
//      dut.clockDomain.waitSampling()
//
//
//    }
//
//
//  }
//}
//
//
//
//
//
//
//
//
//
