package Chainsaw.waveform

import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import java.io._

/** a waveform which will occupy a row in generated figure
 *
 * @see [[https://github.com/wavedrom/schema/blob/master/WaveJSON.md]] for full syntax of "wave" attribute
 */
case class Waveform(name: String, wave: String, data: Seq[String]) {

  def toGlitchFree(wave: String) =
    wave.head +: (1 until wave.length).map { i =>
      val char = wave(i)
      val charPre = wave(i-1)
      if ((char == '0' || char == '1') && char == charPre) '.' else char
    }.mkString("")

  def toJson = Map("name" -> name, "wave" -> toGlitchFree(wave), "data" -> data)
}

case class WaveformGraph(name: String, waves: Seq[Waveform]) {

  def toJson = {

    val period = waves.map(_.wave.length).max
    // add clk at the first cycle as a reference
    "signal" -> (Waveform("clk", "p" + "." * (period - 1), Seq()) +: waves.map(_.toJson))
  }

  def generateJsonFile(): Unit = {

    implicit val format: AnyRef with Formats = Serialization.formats(NoTypeHints)
    val json = write(toJson)
    val jsonFile = new File(s"./$name.json")
    val pw = new PrintWriter(jsonFile)
    pw.write(json)
    pw.close()
    println(s"your waveform file ${jsonFile.getAbsolutePath} had been generated ")
  }

}

object WaveformGraph {
  // user case
  def main(args: Array[String]): Unit = {
    val wave0 = Waveform("w0", "====", (0 until 4).map("x" + _.toString))
    val wave1 = Waveform("w1", "====", (5 until 8).map("x" + _.toString))
    val fig = WaveformGraph("example", Seq(wave0, wave1))
    fig.generateJsonFile()
  }
}
