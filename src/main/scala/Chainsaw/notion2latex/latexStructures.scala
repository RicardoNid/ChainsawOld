package Chainsaw.notion2latex

import java.io.File
import scala.io.Source


object Markdown {

  def parseMd(mdFile:File) = {
    Source.fromFile(mdFile).getLines()
  }

//  def main(args: Array[String]): Unit = {
//    parseMd("tes")
//  }

}