package sysu.util

import java.io.FileWriter
import java.nio.file.Paths

import sysu.CNN._
import sysu.xilinx._

import scala.util.Random

object SeqGenStudy {

  def main(args: Array[String]): Unit = {

    val randGen = new Random(42) // 随机数发生器

    val sequences = Array(
      Array.tabulate(100)(i => i / 2), // 测试模式1,计数器,与counter对照
      Array.tabulate(100)(i => if (i % 13 != 0) (i / 3 + i % 5 + i / 7 + i % 9 + 1) else 0), // 测试模式2,算术序列发生器,与手写实现对照
      Array.ofDim[Int](100).map(_ => randGen.nextInt % 100 + 500), // 测试模式3,随机数列,与查表法比较
      getColArrays(addrSeq(3, 7, 7, 3, 2, 2))(0) // 测试模式4,卷积访存序列,与查表法对照
    )

    // 构造SeqGen的测试Flow
    val flows = (0 until 4).map { i =>
      val moduleName = s"SeqGen_${i}"
      println(sequences(i).mkString(" "))
      VivadoFlow(
        design = new SeqGen(sequences(i)),
        vivadoConfig = recommended.vivadoConfig,
        vivadoTask = VivadoTask(
          topModuleName = moduleName,
          workspacePath = s"output/util/${moduleName}_${i}", taskType = SYNTH),
        force = true)
    }

    val pattern2Flows = (0 until 1).map { i =>
      val moduleName = s"ArithSeqGen_${i}"
      VivadoFlow(
        design = new ArithmeticSeqGen(5, 9, 3, 7, 13, 100),
        vivadoConfig = recommended.vivadoConfig,
        vivadoTask = VivadoTask(topModuleName = moduleName,
          workspacePath = s"output/util/${moduleName}_${i}", taskType = SYNTH),
        force = true)
    }

    val pattern3Flows = (0 until sequences.length).map { i =>
      val moduleName = s"LUTSeqGen_${i}"
      VivadoFlow(
        design = new LUTSeqGen(sequences(i)),
        vivadoConfig = recommended.vivadoConfig,
        vivadoTask = VivadoTask(topModuleName = moduleName, workspacePath = s"output/util/${moduleName}_${i}", taskType = SYNTH),
        force = true)
    }

    val results = flows.map(_.doit()).map(_.getReport)
    val results2 = pattern2Flows.map(_.doit()).map(_.getReport)
    val results3 = pattern3Flows.map(_.doit()).map(_.getReport)
    results.foreach(result => println(result.mkString(" ")))
    results2.foreach(result => println(result.mkString(" ")))
    results3.foreach(result => println(result.mkString(" ")))

    val content = results.map(_.mkString(" ")).mkString("\n") + results2.map(_.mkString(" ")).mkString("\n") + results3.map(_.mkString(" ")).mkString("\n")
    val file = new FileWriter(Paths.get("output/util", "SeqGenReport").toFile)
    file.write(content)
    file.flush();
    file.close();
  }
}
