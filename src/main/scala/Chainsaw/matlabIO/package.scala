package Chainsaw

// to resolve this, File -> project structure -> modules -> Chainsaw -> dependencies, add
// <MatlabPath>/extern/engines/java/jar/engine.jar
// <MatlabPath>/sys/os/glnxa64
// <MatlabPath>/bin/glnxa64

import com.mathworks.engine.MatlabEngine
import com.mathworks.matlab.types

import java.nio.file.Paths
import scala.io.Source

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

package object matlabIO {

  var matlabWorkingSpace = java.nio.file.Paths.get("./matlabWorkspace")

  val AsyncEng = if (MatlabEngine.findMatlab().nonEmpty) {
    MatlabEngine.connectMatlabAsync(MatlabEngine.findMatlab()(0))
  } else MatlabEngine.startMatlabAsync()

  val eng: MatlabEngine = AsyncEng.get()

  type MComplex = types.Complex
  type MStruct = types.Struct
  type MHandleObject = types.HandleObject
  type MCellStr = types.CellStr

  object MComplex {
    def apply(real: Double, imag: Double): MComplex = new MComplex(real, imag)

    def apply(real: Double): MComplex = new MComplex(real, 0)
  }

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

  /** Implement some basic operations of complex number
   *
   * @param complex
   */
  implicit class ComplexUtil(complex: MComplex) {
    def *(that: MComplex): MComplex = new MComplex(
      complex.real * that.real - complex.imag * that.imag,
      complex.real * that.imag + complex.imag * that.real)

    def *(that: Double): MComplex = complex * new MComplex(that, 0)

    def +(that: MComplex) = new MComplex(
      complex.real + that.real,
      complex.imag + that.imag
    )

    def -(that: MComplex) = new MComplex(
      complex.real - that.real,
      complex.imag - that.imag
    )

    def /(that: Double) = new MComplex(
      complex.real / that,
      complex.imag / that
    )

    def formatted(fmtstr: String) = complex.real.formatted(fmtstr) + " + " + complex.imag.formatted(fmtstr) + "i"

    def sameAs(that: MComplex, epsilon: Double = 1.0) = {
      (complex.real - that.real).abs < epsilon &&
        (complex.imag - that.imag).abs < epsilon
    }

    override def equals(obj: Any) = {
      obj match {
        case complex: MComplex => this.sameAs(complex)
        case _ => false
      }
    }

    def toString(length: Int) =
      complex.real.toString.padTo(length, ' ').take(length) + " + " +
        (complex.imag.toString).padTo(length, ' ').take(length) + "i"

    def conj = new MComplex(complex.real, -complex.imag)

    def modulus = scala.math.sqrt(complex.real * complex.real + complex.imag * complex.imag)

    def unary_- = new MComplex(-complex.real, -complex.imag)
  }

  implicit class EngUtil(eng: MatlabEngine) {
    def setWorkingDir(workingDir: String): Unit = eng.eval(s"cd $workingDir")

    def load[T](fileName: String): T = {
      eng.eval(s"load $fileName")
      eng.getVariable(fileName).asInstanceOf[T]
    }

    def load[T](fileName: String, varName: String): T = {
      eng.eval(s"load $fileName")
      eng.getVariable(varName).asInstanceOf[T]
    }

    def getSFixType(variableName: String) = {
      eng.eval(s"temp0 = ${variableName}.numerictype.Signedness;\n" +
        s"temp1 = ${variableName}.numerictype.WordLength;\n" +
        s"temp2 = ${variableName}.numerictype.FractionLength;\n")
      val signedString = eng.getVariable("temp0").asInstanceOf[String]
      if (signedString != "Signed") throw new IllegalArgumentException(s"$variableName is not signed")
      val wordLength = eng.getVariable("temp1").asInstanceOf[Double].toInt
      val fractionLength = eng.getVariable("temp2").asInstanceOf[Double].toInt
      HardType(SFix((wordLength - 1 - fractionLength) exp, -fractionLength exp))
    }

    def getFixRaws(variableName: String) = {
      eng.eval(s"temp = int($variableName);")
      eng.getVariable("temp").asInstanceOf[Array[Int]]
    }
  }
}
