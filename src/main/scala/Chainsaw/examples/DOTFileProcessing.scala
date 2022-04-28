package Chainsaw.examples
import Chainsaw.{logger, _}

/** Regularize DOT to make a prettier picture
  */
class DOTFileProcessing {

  /** Regularize DOT by adding ranks
    * @param DOTContent
    *   DOT content to be regularize
    */
  def DOTGraphRanking(DOTContent: String, rankList: List[String]): String = {
    val ranking = rankList.map(s => "    " + s + ";\n").reduceLeft((s1, s2) => s1 + s2)
    def rankStructure(ranking: String): String = {
      s"""  {
         |    rank=same;
         |$ranking  }""".stripMargin
    }
    val rank = rankStructure(ranking)
    DOTContent.replace("digraph G {", s"digraph G {\n$rank\n")
  }

  /** Add Global Rank Dir
    * @param DOTContent
    *   DOT content to be regularize
    * @param GlobalRankdir
    *   one of "LR", "RL", "TB", "BT"
    * @return
    */
  def addGlobRankDir(DOTContent: String, GlobalRankdir: String = "TB"): String = {
    var GRdir = s"TB"
    GlobalRankdir match {
      case "LR" => GRdir = s"  rankdir = LR;\n"
      case "RL" => GRdir = s"  rankdir = RL;\n"
      case "TB" => GRdir = s"  rankdir = TB;\n"
      case "BT" => GRdir = s"  rankdir = BT;\n"
      case _    => logger.error("Wrong rankdir type")
    }
    DOTContent.replace("digraph G {", s"digraph G {\n\n$GRdir")
  }

}

// Tips: Copy the result str and use VSCode to view the graph
// The VSCode plugin "Graphviz Interactive Preview" is prefered
object DOTFileProcessing {
  def main(args: Array[String]): Unit = {
    // TODO: "FileProcessing" rather than "StrProcessing"
    val str: String =
      """
        |strict digraph G {
        |  a0;
        |  a1;
        |  a2;
        |  b0;
        |  b1;
        |  b2;
        |  a0 -> a1;
        |  a1 -> a2;
        |  b0 -> b1;
        |  b1 -> b2;
        |  a0 -> b0;
        |  a1 -> b1;
        |  a2 -> b2;
        |}
        |""".stripMargin
    val dotProcess = new DOTFileProcessing
    // TODO: STEP1: Styling
    // STEP2 : Group Ranking
    val a = dotProcess.DOTGraphRanking(str, List("a0", "a1", "a2"))
    val b = dotProcess.DOTGraphRanking(a, List("b0", "b1", "b2"))
    // STEP3 : Setting GlobalRankDir
    val strRankDir = dotProcess.addGlobRankDir(b, "LR") // add rankdir lastly
    println(strRankDir)
  }
}
