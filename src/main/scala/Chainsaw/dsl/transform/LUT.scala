//package Chainsaw.dsl.transform
//
//import spinal.core._
//import scala.reflect.ClassTag
//import LUT._
//
//class LUT[T](val lut: Array[T])
//            (implicit classTag: ClassTag[T])
//  extends Transform[Int, T](transformFromLUT(lut), 1, 1)
//
//object LUT {
//
//  def transformFromLUT[T](lut: Array[T])(implicit classTag: ClassTag[T]) =
//    (dataIn: Array[Int]) => {
//      require(dataIn.length == 1)
//      Array(lut(dataIn.head))
//    }
//
//  def buildFromLUT[H <: Data](lut: Array[H])(implicit classTag: ClassTag[H]) = {
//    val ROM =  Mem(lut)
//    (dataIn: UInt) => ROM.readAsync(dataIn)
//  }
//
//
//  def apply[T](lut: Array[T])
//              (implicit classTag: ClassTag[T]): LUT[T] =
//    new LUT(lut)
//
//  def apply[T](lut: T*)
//              (implicit classTag: ClassTag[T]): LUT[T] =
//    new LUT(lut.toArray)
//}
