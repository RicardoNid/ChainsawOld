import java.nio.file.Paths
import scala.io.Source

package object matlabIO {

  var matlabWorkingSpace = java.nio.file.Paths.get("~/").toAbsolutePath

  def writeFile(fileName: String, content: String) = {
    val filepath = Paths.get(matlabWorkingSpace.toString, fileName)
    val writer = new java.io.FileWriter(filepath.toFile)
    writer.write(content)
    writer.flush()
    writer.close()
  }

  def writeFileIncrementally(fileName: String, content: String) = {
    val filepath = Paths.get(matlabWorkingSpace.toString, fileName)
    val oldContent = Source.fromFile(filepath.toString).getLines().mkString("\n")
    val writer = new java.io.FileWriter(filepath.toFile)
    writer.write(oldContent + content)
    writer.flush()
    writer.close()
  }

  def addGlobalParameter(name: String, value: String): Unit = {
    writeFileIncrementally("globals.m", s"global $name; $name = $value")
  }

}
