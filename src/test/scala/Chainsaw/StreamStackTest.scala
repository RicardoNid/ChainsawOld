package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import org.scalatest.funsuite.AnyFunSuite

class StreamStackTest extends AnyFunSuite {
  test("testStreamStack") {
    SimConfig.withWave
      .compile(new StreamStack(UInt(4 bits), 10) {
        logic.popping.simPublic()
        logic.pushing.simPublic()
      })
      .doSim { dut =>
        import dut._
        // init
        clockDomain.forkStimulus(2)
        io.push.valid #= false
        io.pop.ready  #= false
        io.flush      #= false

        fork { // monitor
          while (true) {
            if (logic.popping.toBoolean) println(io.pop.payload.toInt)
            clockDomain.waitSampling()
          }
        }

        clockDomain.waitSampling(2)

        def push(value: Int): Unit = {
          io.push.valid   #= true
          io.push.payload #= value
        }

        def pushBubble(): Unit = {
          io.push.valid   #= false
          io.push.payload #= ChainsawRand.nextInt(16)
        }

        def pop() = {
          io.pop.ready #= true
        }

        def popBubble(): Unit = {
          io.pop.ready #= false
        }

        // no gap
        push(3)
        pop()
        clockDomain.waitSampling()
        pushBubble()
        popBubble()
        clockDomain.waitSampling()
        // 1 cycle gap
        push(4)
        popBubble()
        clockDomain.waitSampling()
        pushBubble()
        pop()
        clockDomain.waitSampling()
        pushBubble()
        popBubble()
        clockDomain.waitSampling()
        // 2 cycle gap
        push(5)
        popBubble()
        clockDomain.waitSampling()
        pushBubble()
        popBubble()
        clockDomain.waitSampling()
        pushBubble()
        pop()
        clockDomain.waitSampling()
        pushBubble()
        popBubble()
        clockDomain.waitSampling()

        (0 until 15).foreach { i =>
          push(i)
          popBubble()
          clockDomain.waitSampling()
        }

        (0 until 15).foreach { _ =>
          pushBubble()
          pop()
          clockDomain.waitSampling()
        }
      }
  }

}
