package FTN

import Chainsaw.{DSPRand, eng}
import com.mathworks.engine.MatlabEngine
import com.mathworks.matlab.types._
import scala.collection.JavaConversions._
import matlabIO._



object MatlabRef {
  val eng = AsyncEng.get()
  def poly2trellis(constLen: Int, codeGen: Array[Int]): MStruct = eng.feval[MStruct]("poly2trellis", Array(constLen.toDouble), codeGen.map(_.toDouble))
  def vitdec(bits: Array[Double], trellis: MStruct, tblen: Int): Array[Double] =
    eng.feval[Array[Double]]("vitdec", bits, trellis, Array(tblen.toDouble), "cont", "hard")
  def convenc(bits:Array[Double], trellis: MStruct): Array[Double] =
    eng.feval[Array[Double]]("convenc", bits, trellis)

  // FIXME: fake API
  def convenc(bits:Array[Double], config: ConvencConfig): Array[Double] =
    eng.feval[Array[Double]]("convenc", bits, config)
}
