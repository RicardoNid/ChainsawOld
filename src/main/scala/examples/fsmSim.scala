import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

object StateMachineSimExample {
  class TopLevel extends Component {
    val counter = out(Reg(UInt(8 bits)) init (0))
    val cond = in Bool
    val fsm = new StateMachine {
      setTransitionCondition(cond)
      val stateA, stateB, stateC = new State
      setEntry(stateA)
      stateA.whenIsActive {
        goto(stateB)
      }
      stateB.whenIsActive {
        forceGoto(stateC)
      }
      stateC.onEntry(counter := 0)
      stateC.whenIsActive {
        when(counter === 3) {
          goto(stateA)
        }.otherwise {
          counter := counter + 1
        }
      }
    }
  }

  def main(args: Array[String]) {
    import spinal.core.sim._
    SimConfig.withWave.compile{
      val dut = new TopLevel
      dut.fsm.stateReg.simPublic()
      dut.fsm.stateNext.simPublic()
      dut.fsm.stateNextCand.simPublic()
      dut
    }.doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      dut.cond #= false
      for(i <- 0 until 20){
        dut.clockDomain.waitSampling()
        dut.cond #= !dut.cond.toBoolean
        println(f"State: ${dut.fsm.stateReg.toEnum} StateNext: ${dut.fsm.stateNext.toEnum} Cand: ${dut.fsm.stateNextCand.toEnum} Cond: ${dut.cond.toBoolean}")
      }
    }
  }
}