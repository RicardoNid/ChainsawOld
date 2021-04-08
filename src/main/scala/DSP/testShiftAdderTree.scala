package DSP

import breeze.numerics.pow
import spinal.core._
import spinal.core.sim._
import spinal.lib.{master, slave}
import xilinx.VivadoFlow

import scala.util.Random

//  LUT using SInt
class ShiftAdderTreeDUT(shifts: IndexedSeq[Int]) extends Component {

  val inputs = slave Flow Vec(SInt(8 bits), shifts.length)
  val output = master Flow SInt //  bitWidth can be determined later

  output.payload := ShiftAdderTree(inputs.payload.map(_.toSFix), shifts).toSInt
  output.valid := RegNext(inputs.valid)
  output.valid.init(False)
}

//  DUT using SFix
//class ShiftAdderTreeDUT(shifts: IndexedSeq[Int]) extends Component {
//
//  val inputs = slave Flow Vec(data, shifts.length)
//  val naturalBitGrowth = shifts.max + log2Up(shifts.length)
//  val output = master Flow SFix((data.maxExp + naturalBitGrowth) exp, (data.bitCount + naturalBitGrowth) bits) //  bitWidth can be determined later
//
//  val shiftAdderTree = ShiftAdderTree(inputs.payload, shifts)
//  output.payload := shiftAdderTree.implicitValue.truncated
//  output.valid := RegNext(inputs.valid)
//  output.valid.init(False)
//}

object ShiftAdderTreeDUT {
  def main(args: Array[String]): Unit = {
    VivadoFlow(
      design = new ShiftAdderTreeDUT(Array(1, 3, 5, 7, 8)),
      topModuleName = "shiftAdderTree",
      workspacePath = "output/shiftAdderTree",
      force = true).doit()
  }
}

class testShiftAdderTree(shifts: IndexedSeq[Int]) extends ShiftAdderTreeDUT(shifts) with DSPSim {
  override type TestCase = IndexedSeq[Double]
  override type ResultType = Double

  override def simInit(): Unit = {
    clockDomain.forkStimulus(2)
    inputs.valid #= false
    clockDomain.waitSampling(10)
  }

  override def simDone(): Unit = {
    clockDomain.waitSampling(10)
    while (refResults.nonEmpty || dutResults.nonEmpty) clockDomain.waitSampling(10)
  }

  override def driver(): Unit = {
    val drv = fork {
      while (true) {
        if (testCases.nonEmpty) {
          val testCase = testCases.dequeue()
          referenceModel(testCase)
          inputs.valid #= true
          //          println("test: " + testCase.mkString(" "))
          //          (0 until shifts.length).foreach(i => inputs.payload(i).raw #= Double2Fix(testCase(i)))
          (0 until shifts.length).foreach(i => inputs.payload(i) #= testCase(i).toInt)
          clockDomain.waitSampling()
          inputs.valid #= false
        }
        else clockDomain.waitSampling()
      }
    }
  }

  override def referenceModel(testCase: TestCase): Unit = {
    //    val golden = (0 until shifts.length).map(i => testCase(i) * pow(2, shifts(i))).sum
    val golden = (0 until shifts.length).map(i => testCase(i).toInt * pow(2, shifts(i))).sum
    refResults.enqueue(golden)
  }

  override def monitor(): Unit = {
    val mon = fork {
      while (true) {
        if (output.valid.toBoolean) {
          //          val dutResult = Fix2Double(output.payload)
          val dutResult = output.payload.toInt
          dutResults.enqueue(dutResult)
        }
        clockDomain.waitSampling()
      }
    }
  }

  override def scoreBoard(): Unit = {

    val score = fork {
      while (true) {
        if (refResults.nonEmpty && dutResults.nonEmpty) {
          val refResult = refResults.dequeue()
          val dutResult = dutResults.dequeue()
          if (!sameFixed(refResult, dutResult)) println(s"\n ref: $refResult \n dut: $dutResult")
          //          if (sameFixed(refResult, dutResult)) println(s"\n ref: $refResult \n dut: $dutResult")
          //          assert(sameFixed(refResult, dutResult), s"\n$refResult \n$dutResult")
        }
        clockDomain.waitSampling()
      }
    }
  }
}

object testShiftAdderTree {
  private val r = Random

  def randomShifts = {
    val length = r.nextInt(32) + 5
    val res = (0 until length).map(i => r.nextInt(10))
    println(res.mkString(" "))
    res
  }

  def randomCase(length: Int) = (0 until length).map(i => randData()).toArray

  def randomSim(shifts: IndexedSeq[Int]): Unit = {
    val dut = SimConfig.withWave.compile(new testShiftAdderTree(shifts))
    dut.doSim { dut =>
      dut.sim()
      for (i <- 0 until 100) dut.insertTestCase(randomCase(shifts.length))
      dut.simDone()
    }
    print(Console.GREEN)
    println(s"shiftAdderTree with ${shifts.mkString(" ")}, PASS")
    print(Console.BLACK)
  }

  def main(args: Array[String]): Unit = {
    (0 until 10).foreach(i => randomSim(randomShifts))
  }
}







