package Chainsaw.examples
import Chainsaw.{logger, _}

/** Regularize DOT to make a prettier picture
 */
class DOTFileProcessing {

  /** Regularize DOT by adding ranks
   * @param DOTContent DOT content to be regularize
   */
  def DOTGraphRanking(DOTContent:String): String = {

    // TODO: make it adjustable
    // just for testing
    val rank0 =
      s"""  {
         |    rank=same;
         |    a0;
         |    a1;
         |    a2
         |  }""".stripMargin

    val rank1 =
      s"""  {
         |    rank=same;
         |    b0;
         |    b1;
         |    b2
         |  }""".stripMargin

    DOTContent.replace("digraph G {", s"digraph G {\n$rank0\n\n$rank1\n")
  }

  def addRankDir(DOTContent:String, GlobalRankdir:String = "TB"): String = {
    var GRdir = s"TB"
    GlobalRankdir match {
      case "LR" => GRdir = s"  rankdir = LR;\n"
      case "RL" => GRdir = s"  rankdir = RL;\n"
      case "TB" => GRdir = s"  rankdir = TB;\n"
      case "BT" => GRdir = s"  rankdir = BT;\n"
      case _ => logger.error("Wrong rankdir type")
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
    val strRanked = dotProcess.DOTGraphRanking(str)
    // STEP3 : Setting GlobalRankDir
    val strRankDir = dotProcess.addRankDir(strRanked, "LR")    // add rankdir lastly
    println(strRankDir)
  }
}