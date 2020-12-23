package tutorial.HDLB.lemmings

import spinal.core._

class Lemmings3 extends Component {
  val io = new Bundle {
    val clk = in Bool
    val areset = in Bool
    val bump_left = in Bool
    val bump_right = in Bool
    val ground = in Bool
    val dig = in Bool
    val walk_left = out Bool
    val walk_right = out Bool
    val aaah = out Bool
    val digging = out Bool
  }

  object walkingType extends SpinalEnum {
    val walkingS, walkRightS, fallingS, diggingS = newElement()
  }

  object directionType extends SpinalEnum {
    val leftS, rightS = newElement()
  }

  import directionType._
  import walkingType._

  val clockConfig = ClockDomainConfig(resetKind = ASYNC)
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.areset, config = clockConfig)) {
    val walkingStateNext = walkingType()
    val walkingState = RegNext(walkingStateNext) init (walkingS)

    walkingStateNext := walkingState
    switch(walkingState) {
      is(walkingS) {
        when(!io.ground) {
          walkingStateNext := fallingS
        }.elsewhen(io.dig) {
          walkingState := diggingS
        }
      }
      is(fallingS) {
        when(io.ground) {
          walkingStateNext := walkingS
        }
      }
      is(diggingS) {
        when(!io.ground) {
          walkingStateNext := fallingS
        }
      }
    }

    val directionStateNext = directionType()
    val directionState = RegNext(directionStateNext) init (leftS)

    directionStateNext := directionState
    switch(directionState) {
      is(leftS) {
        when(io.ground && !io.dig && walkingState === walkingS && io.bump_left) {
          directionStateNext := rightS
        }
      }
      is(rightS) {
        when(io.ground && !io.dig && walkingState === walkingS && io.bump_right) {
          directionStateNext := leftS
        }
      }
    }

    io.walk_left := (walkingState === walkingS && directionState === leftS)
    io.walk_right := (walkingState === walkingS && directionState === rightS)
    io.aaah := (walkingState === fallingS)
    io.digging := (walkingState === diggingS)
  }
  noIoPrefix()
}

object Lemmings3 {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Lemmings3().setDefinitionName("top_module"))
  }
}
