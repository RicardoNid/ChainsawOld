package Chainsaw.examples


import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import java.io._
import scala.io.Source

import Chainsaw._

object JsonExample extends App {

  val temp = Seq(1,2,3)
  implicit val format = Serialization.formats(NoTypeHints)
  val json = write(temp)
  val jsonFile = "./temp.json"
  val pw = new PrintWriter(new File(jsonFile))
  pw.write(json)
  pw.close()
  val recoveredJson = Source.fromFile(jsonFile).getLines().mkString("")
  val recoveredData = read[Seq[Int]](recoveredJson)
  println(recoveredData.mkString(" "))



}
