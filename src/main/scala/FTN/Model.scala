package FTN

import Chainsaw._

object Model {
  // Run matlab model
  def main(args: Array[String]): Unit = {
    eng.eval("cd /home/ltr/IdeaProjects/Chainsaw/src/main/scala/FTN/Matlab")
    eng.eval("FTN")
    eng.close()
  }
}