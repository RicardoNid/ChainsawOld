package projects.Wht

import spinal.core._
import spinal.core.sim._
import spinal.lib.fsm._
import sysu.xilinx._

class Wht extends Component {

  val ori = Array.tabulate(32, 16)(_ * 16 + _)
  val map1 = (i: Int) => if (i > 15 && (i % 16 != 0)) i - 17 else -1
  val map2 = (i: Int) => if (i > 15) i - 16 else -1
  val map3 = (i: Int) => if (i % 16 != 0) i - 1 else -1
  // 原始数据及其依赖项
  val source = ori.map(_.map(i => Array(i, map1(i), map2(i), map3(i)))).flatten
  // 原始数据的实际顺序
  val reorder = ori.flatten.sortBy(i => ((i / 16) + (i % 16)) * 16 + (i / 16))
  // 按顺序重排的映射
  val remap = reorder.zipWithIndex.map {
    case (ele, i) => ele -> i
  }.toMap


  val stream = source.map(_.map(i => if (i >= 0) remap(i) else -1)).sortBy(_ (0))
  stream.map(_.mkString(" ")).mkString("\n")
  val switch = stream.map(_.map(i => if (i >= 0) 1 else 0))
  switch.map(_.mkString(" ")).mkString("\n")
  // 原始数据机器依赖项的delay关系
  val reuse = stream.zip(switch).map {
    case (row1, row2) => {
      Array(
        (row1(0) - row1(1)) * row2(1),
        (row1(0) - row1(2)) * row2(2),
        (row1(0) - row1(3)) * row2(3))
    }
  }
  //  println(reuse.map(_.mkString(" ")).mkString("\n"))
  //reuse.length = 512
  //reuse.map(_.max).max 最大重用距离 = 31
  reuse.map(_.count(_ == 31)).sum

  val seq = reuse.map(_ (0)).toArray


  val io = new Bundle {
    val valid = out Bool
    val delay = out UInt (6 bits)
//    val test = out UInt (6 bits)
//    val compare = out UInt (6 bits)
//    val correct = out Bool
  }

  val countInner = Reg(UInt(5 bits)) init (0)
  val countOuter = Reg(UInt(5 bits)) init (1)

//  val lutGen = new LUTSeqGen(seq)

  val fsm = new StateMachine {
    val WAIT = new StateDelay(2).whenCompleted(goto(S0))
    setEntry(WAIT)
    val S0 = State()
    val S1 = new StateDelay(15 * 17 + 16 - 1)
    val S2 = State()
    val DONE = State()

    io.delay := countOuter << 1
    io.valid := False

    S0
      .whenIsActive {
        when(countOuter < 15) {
          when(countOuter === countInner) {
            countOuter := countOuter + 1
            countInner := 0
          }
            .otherwise(countInner := countInner + 1)
        }
          .otherwise {
            when(countOuter === countInner) {
              countInner := 0
              goto(S1)
            }
              .otherwise(countInner := countInner + 1)
          }
        io.valid := (countInner < countOuter - 1)
      }
      .onExit {
        io.delay := 31 // todo
        io.valid := True
      }

    S1
      .onEntry {
        io.delay := 31 // todo
        io.valid := True
      }
      .whenIsActive {
        when(countInner === 15)(countInner := 0)
          .otherwise(countInner := countInner + 1)
        io.valid := countInner =/= 14
        io.delay := 31
      }
      .onExit {
        io.delay := 30 // todo
        io.valid := True
      }
      .whenCompleted {
        goto(S2)
      }

    S2
      .onEntry {
        io.delay := 31 // todo
        io.valid := True
        countInner := 0
      }
      .whenIsActive {
        when(countInner === countOuter - 2) {
          countOuter := countOuter - 1
          countInner := 0
          when(countOuter === 2)(goto(DONE))
        }
          .otherwise(countInner := countInner + 1)
        io.valid := True
      }

    DONE
      .whenIsActive {
        io.delay := 0
        io.valid := False
      }
  }
//  when(io.valid)(io.test := io.delay).otherwise(io.test := 0)
//  io.compare := lutGen.io.output.resized
//  io.correct := (io.compare === io.test)
}

object Wht {
  def main(args: Array[String]): Unit = {
    val report = VivadoFlow(
      design = new Wht,
      vivadoConfig = recommended.vivadoConfig,
      vivadoTask = VivadoTask(topModuleName = "projects/Wht", workspacePath = "output/projects.Wht"),
      force = true
    ).doit()
    report.printArea
    report.printFMax
  }
}

object testWht {
  def main(args: Array[String]): Unit = {
    val period = 2
    SimConfig.withWave.compile(new Wht()).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        for (i <- 0 until 600) {
          sleep(period)
        }
        simSuccess()
      }
  }
}
