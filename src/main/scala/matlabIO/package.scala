import com.mathworks.engine.MatlabEngine
import com.mathworks.matlab.types
import com.mathworks.matlab.types._

import java.nio.file.Paths
import scala.io.Source

package object matlabIO {

  var matlabWorkingSpace = java.nio.file.Paths.get("/home/ltr/IdeaProjects/Chainsaw/matlabWorkspace")

  val AsyncEng = if (MatlabEngine.findMatlab().nonEmpty) {
    MatlabEngine.connectMatlabAsync(MatlabEngine.findMatlab()(0))
  } else MatlabEngine.startMatlabAsync()

  type MComplex = types.Complex
  type MStruct = types.Struct
  type MHandleObject = types.HandleObject
  type MCellStr = types.CellStr

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

  implicit class ArrayUtil(array: Array[_]) {
    def info: String = { // valid for "pure" array only
      var typeString = ""
      def recursiveBuild(current: Any, ret: Seq[String] = Seq[String]()): Seq[String] = {
        current match {
          case array: Array[_] => recursiveBuild(array(0), ret :+ array.size.toString)
          case value => {
            typeString = value.getClass.toString
            ret
          }
        }
      }
      s"[${recursiveBuild(array).mkString("x")} ${typeString.split('.').last}]"
    }

    def formatted: String = {
      var deepest = 0
      def recursiveBuild(element: Any, depth: Int = 0): String = {
        deepest = deepest max depth
        def sep = if ((deepest - depth) == 1) " " else "\n" * (deepest - depth - 1)
        element match {
          case array: Array[_] => array.map(recursiveBuild(_, depth + 1)).mkString(sep)
          case _ => element.toString
        }
      }
      recursiveBuild(array)
    }
  }

  implicit class StructUtil(struct: MStruct) {
    def formatted = {
      struct.keySet().toArray.zip(struct.values().toArray).map { case (key, value) =>
        val valueString = value match {
          case array: Array[_] => array.info
          case value => value.toString
        }
        s"$key: $valueString"
      }.mkString("\n")
    }
  }

  implicit class MatlabArray[T](array: Seq[T]) {
    def asMatlab = "[" + array.mkString(", ") + "]"
  }

  implicit class MatlabArray2[T](array: Seq[Seq[T]]) {
    def asMatlab = "[" + array.map(_.mkString(", ")).mkString("; ") + "]"
  }

  val epsilon = 1e-4

  implicit class ComplexUtil(complex: MComplex) {
    def *(that: MComplex) = new MComplex(
      complex.real * that.real - complex.imag * complex.imag,
      complex.real * that.imag + complex.imag * that.real)

    def +(that: MComplex) = new MComplex(
      complex.real + that.real,
      complex.imag - that.imag
    )

    def -(that: MComplex) = new MComplex(
      complex.real - that.real,
      complex.imag - that.imag
    )

    def sameAs(that: MComplex) =
      (complex.real - that.real).abs < epsilon &&
        (complex.imag - that.imag).abs < epsilon

    def conj = new MComplex(complex.real, -complex.imag)

    def unary_- = new MComplex(-complex.real, -complex.imag)
  }
}
