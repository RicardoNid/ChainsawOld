package Chainsaw.notion2latex

import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object Markdown {

  val patterns = Map(
    "# " -> convertH1,
    "## " -> convertH2,
    "### " -> convertH3,
    "1. " -> convertNumber,
    "-  " -> convertBullet,
    "$$\n" -> convertBlockEquation,
    "- [x]  图" -> convertFigure,
    "- [x]  代码段" -> convertCode,
    "| " -> convertTable
  )

  def buildLatexOne(element: String, content: String) = s"\\$element{$content}"

  def buildLatexTwo(element: String, content: String) = s"\\begin{$element}\n$content\n\\end{$element}"

  def labelParser(string: String, dropNum: Int) = {

    val temp = string.drop(dropNum)
    val number = temp.split(" ")(0)
    val chap = number.split("-")(0)
    val index = number.split("-")(1)
    val caption = temp.split(" ")(1)

    (chap, index, caption)
  }

  val convertTitle = (string: String) => buildLatexOne("chapter", string.trim.replace("# ", ""))
  val convertH1 = (string: String) => buildLatexOne("section", string.trim.replace("# ", ""))
  val convertH2 = (string: String) => buildLatexOne("subsection", string.trim.replace("## ", ""))
  val convertH3 = (string: String) => buildLatexOne("pmb", string.trim.replace("### ", ""))

  val convertBlockEquation = (string: String) => buildLatexTwo("equation", string.replace("$$\n", "").trim)
  val convertNumber = (string: String) => string.split("\n").map(_.drop(2)).zipWithIndex.map { case (str, i) => s"(${i + 1}) ${replaceCite(str)}" }.mkString("\n\n")
  val convertBullet = (string: String) => string.split("\n").map(_.drop(2)).zipWithIndex.map { case (str, i) => s"(${i + 1}) ${replaceCite(str)}" }.mkString("\n\n")

  val convertTable = (string: String) => {

    val lines = string.split("\n")
    val (chap, index, caption) = labelParser(lines.last, 8)

    val contents = lines.init
    val elements = contents.map(_.split("\\|").drop(1))

    def toLine(ss: Array[String]) = ss.mkString(" & ") + "\\\\"

    def toTable(ss: Array[Array[String]]) = ss.map(toLine).mkString("\n\\hline\n")

    val table = s"\n\\begin{table}[h]" +
      s"\n\t\\renewcommand\\arraystretch{1.2}" +
      s"\n\t\\centering" +
      s"\n\t\\caption{$caption}" +
      s"\n\t\\label{tab${chap}_$index}" +
      s"\n\t\\begin{tabular}{${Seq.fill(elements.tail.head.length)("c").mkString("|")}}" +
      s"\n\t\t\\toprule[2pt]" +
      s"\n\t\t${toLine(elements.head)}" +
      s"\n\t\t\\midrule[2pt]" +
      s"${toTable(elements.tail.tail)}" +
      s"\n\t\t\\bottomrule[2pt]" +
      s"\n\t\\end{tabular}" +
      s"\n\\end{table}"

    println(s"find table $chap-$index $caption, cols = ${elements.tail.head.length}")
    table
  }

  val convertCode = (string: String) => {

    val (chap, index, caption) = labelParser(string, 10)
    val filename = s"/data/thesis_latex/snippet/${chap}_$index.scala"
    //    println(s"find code $chap-$index $caption")
    val exist = new File(filename).exists()
    if (!exist) {
      println(s"need code $filename")
      ""
    }
    else {
      val sourceCode = Source.fromFile(filename).getLines().mkString("\n")
      s"\\begin{lstlisting}\n$sourceCode\n\\end{lstlisting}"
    }

  }
  val convertFigure = (string: String) => {

    val (chap, index, caption) = labelParser(string, 8)

    def includeFig(chap: String, index: String, caption: String): String = {
      s"""
         |\\begin{figure}[h]
         |	\\centering
         |	\\includegraphics[width=0.5\\textwidth]{figure/Chap$chap/${chap}_$index.png}
         |	\\caption{${caption.trim}}
         |	\\label{fig${chap}_$index}
         |\\end{figure}
         |""".stripMargin
    }

    val filename = s"/data/thesis_latex/figure/Chap$chap/${chap}_$index.png"
    val exist = new File(filename).exists()
    if (!exist) {
      println(s"need fig $filename")
      ""
    }
    else {
      includeFig(chap, index, caption)
    }
  }

  val refs = Source.fromFile("/data/thesis_latex/Bibliography/main.bbl").getLines().mkString(" ")

  def replaceCite(string: String) = {
    val citePattern = "\\[.*?\\]".r
    val cites = citePattern.findAllIn(string)
    var ret = string
    cites.foreach { cite =>
      if (cite != "[Int]" && cite != "[T]" && cite != "[]" && cite != "[-1,1]" && cite != "[n]" && cite != "[x]" && cite != "[1]") {
        val content = cite.drop(1).dropRight(1)
        if (content.nonEmpty) ret = ret.replace(cite, s"\\cite{$content}")
      }
    }
    ret
  }

  val convertText = (string: String) => replaceCite(string)

  def parseBlock(string: String) = {
    val ret = patterns.keys.dropWhile(!string.startsWith(_)).headOption
    val convert = ret match {
      case Some(key) => patterns(key)
      case None => convertText
    }
    convert(string)
  }

  def parseMd(mdFile: String) = {

    val lines = Source.fromFile(mdFile).getLines().toArray

    val contents = ArrayBuffer[String]("")
    lines.foreach { line =>
      if (line.isEmpty) contents += ""
      else contents.update(contents.length - 1, contents.last + line + "\n")
    }

    val ret = (convertTitle(contents.head) +: contents.tail.map(parseBlock))
      .map(_.trim).mkString("\n\n")
    ret
  }


  def main(args: Array[String]): Unit = {

    def convert(index: Int) = {
      val pw = new PrintWriter(new File(s"/data/thesis_latex/docs/Chapter_0$index.tex"))
      pw.write(parseMd(s"/data/thesis_latex/md/Chap$index.md"))
      pw.close()
    }

    convert(1)
    convert(2)
    convert(3)
    convert(4)
    convert(5)

  }
}