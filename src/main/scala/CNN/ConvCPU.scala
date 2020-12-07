package CNN

import sys.process._

class ConvCPU {

}

object ConvCPU {
  def main(args: Array[String]): Unit = {
    val proc1 = Runtime.getRuntime().exec("python3 C:\\Users\\lsfan\\Documents\\GitHub\\SpinalTemplate\\src\\main\\scala\\CNN\\conv.py")
    proc1.waitFor()
    // exec with parameters
  }
}
