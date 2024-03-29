package Chainsaw.algos
import java.io._
import java.nio.file.Paths
import java.nio.ByteOrder
import javax.imageio.stream._
import scala.collection.mutable.ArrayBuffer
import PathInfo._

object PathInfo {
  val bootPath      = "/home/lqx/Sampling_src/"
  val cCodePath     = bootPath + "c_src/"
  val cTestPath     = bootPath + "test_bmp_4K/"
  val scalaTestPath = bootPath + "test_scala/"
  val cUpPath       = cTestPath + "bicubic_upsampling/"
  val scalaUpPath   = scalaTestPath + "fastNedi_upsampling/"
  val cDownPath     = cTestPath + "downsampling/"
  val scalaDownPath = scalaTestPath + "downsampling/"
}

case class BMPPixels(width: Int, height: Int) {
  var r, g, b, a: Array[Short] = Array.fill((width * height).toInt)(0.toShort)
}

case class BMPHeader(width: Long, height: Long) {
  var b_type: Int            = 0x4d42 // Magic identifier: 0x4d42
  var size: Long             = width * height * 3 + 54 // File size in bytes
  var reserved1: Int         = 0 // Not used
  var reserved2: Int         = 0 // Not used
  var offset: Long           = 54 // Offset to image data in bytes from beginning of file (54 bytes)
  var dib_header_size: Long  = 40 // DIB Header size in bytes (40 bytes)
  var width_px: Int          = width.toInt // Width of the image
  var height_px: Int         = height.toInt // Height of image
  var num_planes: Int        = 1 // Number of color planes
  var bits_per_pixel: Int    = 24 // Bits per pixel
  var compression: Long      = 0 // Compression type
  var image_size_bytes: Long = width * height * 3 // Image size in bytes
  var x_resolution_ppm: Int  = 0 // Pixels per meter
  var y_resolution_ppm: Int  = 0 // Pixels per meter
  var num_colors: Long       = 0 // Number of colors
  var important_colors: Long = 0 // Important colors
}

case class BMPImage(width: Int, height: Int) {
  val header: BMPHeader = BMPHeader(width, height)
  val pixels: BMPPixels = BMPPixels(width, height)

  def byteToShort(srcByte: Byte): Short = {
    (srcByte & 0xff).toShort
  }

  def byteToInt(byteArray: Array[Byte]): Int = {
    assert(byteArray.length >= 2, "the byteArray which to be transform to Int is illegal array!\n")
    0xff00 & byteArray(1) << 8 | byteArray(0) & 0xff
  }

  def byteToLong(byteArray: Array[Byte]): Long = {
    assert(byteArray.length >= 4, "the byteArray which to be transform to Long is illegal array!\n")
    0xff000000L & byteArray(3) << 24 | 0xff0000L & byteArray(2) << 16 |
      0xff00L & byteArray(1)   << 8 | 0xffL & byteArray(0)
  }

  def combByteToInt(byteArray: Array[Byte]): Int = {
    assert(byteArray.length >= 4, "the byteArray which to be combined is illegal array!\n")
    0xff000000 & byteArray(3) << 24 | 0xff0000 & byteArray(2) << 16 |
      0xff00 & byteArray(1)   << 8 | 0xff & byteArray(0)
  }

  def readHeader(fileName: String, filePath: String = scalaDownPath, print: Boolean = false) = {
    val headerStream = new FileImageInputStream(Paths.get(filePath, fileName).toFile)

    if (print) { println(s"************The BMP header information************") }
    // read b_type
    val btArray = new Array[Byte](2)
    val rBtNumb = headerStream.read(btArray, 0, 2)
    assert(rBtNumb == 2, "read b_type fail!\n")
    header.b_type = byteToInt(btArray)
    assert(header.b_type == 0x4d42, "illegal bmp file !\n")
    if (print) { println(f"type : 0x${header.b_type}%x") }

    // read size
    val szArray = new Array[Byte](4)
    val rSzNumb = headerStream.read(szArray, 0, 4)
    assert(rSzNumb == 4, "read size fail!\n")
    header.size = byteToLong(szArray)
    if (print) { println(f"size : ${header.size}%d") }

    // read reserved
    val reArray = new Array[Byte](4)
    val rReNumb = headerStream.read(reArray, 0, 4)
    assert(rReNumb == 4, "read reserved fail!\n")
    if (print) {
      println(f"reserved1 : ${header.reserved1}%d")
      println(f"reserved2 : ${header.reserved2}%d")
    }

    // read offset
    val osArray = new Array[Byte](4)
    val rOsNumb = headerStream.read(osArray, 0, 4)
    assert(rOsNumb == 4, "read offset fail!\n")
    header.offset = byteToInt(osArray)
    if (print) { println(f"offset : ${header.offset}%d") }

    // read dib_header_size
    val dibArray = new Array[Byte](4)
    val rDibNumb = headerStream.read(dibArray, 0, 4)
    assert(rDibNumb == 4, "read dib_header_size fail!\n")
    header.dib_header_size = byteToLong(dibArray)
    if (print) { println(f"dib_header_size : ${header.dib_header_size}%d") }

    // read width_px/height_px
    val widArray = new Array[Byte](4)
    val rWidNumb = headerStream.read(widArray, 0, 4)
    assert(rWidNumb == 4, "read width_px fail!\n")
    header.width_px = combByteToInt(widArray)
    if (print) { println(f"width_px : ${header.width_px}%d") }
    val heiArray = new Array[Byte](4)
    val rHeiNumb = headerStream.read(heiArray, 0, 4)
    assert(rHeiNumb == 4, "read height_px fail!\n")
    header.height_px = combByteToInt(heiArray)
    if (print) { println(f"height_px : ${header.height_px}%d") }

    // read num_plane
    val numPArray = new Array[Byte](2)
    val rNumPNumb = headerStream.read(numPArray, 0, 2)
    assert(rNumPNumb == 2, "read num_plane fail!\n")
    header.num_planes = byteToInt(numPArray)
    if (print) { println(f"num_planes : ${header.num_planes}%d") }

    // read bits_per_pixels
    val bitsArray = new Array[Byte](2)
    val rBitsNumb = headerStream.read(bitsArray, 0, 2)
    assert(rBitsNumb == 2, "read bits_per_pixels fail!\n")
    header.bits_per_pixel = byteToInt(bitsArray)
    if (print) { println(f"bits_per_pixels : ${header.bits_per_pixel}%d") }

    // read compression
    val compArray = new Array[Byte](4)
    val rComPNumb = headerStream.read(compArray, 0, 4)
    assert(rComPNumb == 4, "read compression fail!\n")
    header.compression = byteToLong(compArray)
    if (print) { println(f"compression : ${header.compression}%d") }

    // read image_size_bytes
    val imgArray = new Array[Byte](4)
    val rImgNumb = headerStream.read(imgArray, 0, 4)
    assert(rImgNumb == 4, "read image_size_bytes fail!\n")
    byteToLong(imgArray) match {
      case 0L => header.image_size_bytes = header.size - header.offset
      case _  => header.image_size_bytes = byteToLong(imgArray)
    }
    if (print) { println(f"image_size_bytes : ${header.image_size_bytes}%d") }

    // read x_resolution_ppm/ y_resolution_ppm
    val xreArray = new Array[Byte](4)
    val rXReNumb = headerStream.read(xreArray, 0, 4)
    assert(rXReNumb == 4, "read x_resolution_ppm fail!\n")
    header.x_resolution_ppm = combByteToInt(xreArray)
    if (print) { println(f"x_resolution_ppm : ${header.x_resolution_ppm}%d") }

    val yreArray = new Array[Byte](4)
    val rYReNumb = headerStream.read(yreArray, 0, 4)
    assert(rYReNumb == 4, "read y_resolution_ppm fail!\n")
    header.y_resolution_ppm = combByteToInt(yreArray)
    if (print) { println(f"y_resolution_ppm : ${header.y_resolution_ppm}%d") }

    // read num_colors
    val numCArray = new Array[Byte](4)
    val rNumCNumb = headerStream.read(numCArray, 0, 4)
    assert(rNumCNumb == 4, "read num_colors fail!\n")
    header.num_colors = byteToLong(numCArray)
    if (print) { println(f"num_colors : ${header.num_colors}%d") }

    // read important_colors
    val impArray = new Array[Byte](4)
    val rImpNumb = headerStream.read(impArray, 0, 4)
    assert(rImpNumb == 4, "read important_colors fail!\n")
    header.important_colors = byteToLong(impArray)
    if (print) { println(f"important_colors : ${header.important_colors}%d") }

    headerStream.close()
  }

  def readPixels(fileName: String, filePath: String = scalaDownPath) = {
    val pixelStream = new FileImageInputStream(Paths.get(filePath, fileName).toFile)
    // skip header's byte
    val skipNumb = pixelStream.skipBytes(header.offset)
    assert(skipNumb == header.offset, "Fail to skip!\n")

    (0 until (header.image_size_bytes * 8 / header.bits_per_pixel).toInt).foreach { idx =>
      pixels.b(idx) = byteToShort(pixelStream.readByte())
      //println(pixels.b(idx))
      pixels.g(idx) = byteToShort(pixelStream.readByte())
      //println(pixels.g(idx))
      pixels.r(idx) = byteToShort(pixelStream.readByte())
      //println(pixels.r(idx))
      if (header.bits_per_pixel == 32) {
        pixels.a(idx) = byteToShort(pixelStream.readByte())
        //println(pixels.a(idx))
      }
    }
    pixelStream.close()

  }
  def bmpRead(fileName: String, filePath: String = scalaDownPath, print: Boolean = false): Unit = {
    readHeader(fileName, filePath, print)
    readPixels(fileName, filePath)
  }

  def bmpWrite(fileName: String, filePath: String = scalaUpPath): Unit = {
    val retStream = new FileImageOutputStream(Paths.get(filePath, fileName).toFile)

    // write header
    retStream.setByteOrder(ByteOrder.LITTLE_ENDIAN)
    retStream.writeShort(header.b_type)
    retStream.writeInt(header.size.toInt)
    retStream.writeShort(header.reserved1)
    retStream.writeShort(header.reserved2)
    retStream.writeInt(header.offset.toInt)
    retStream.writeInt(header.dib_header_size.toInt)
    retStream.writeInt(header.width_px)
    retStream.writeInt(header.height_px)
    retStream.writeShort(header.num_planes)
    retStream.writeShort(header.bits_per_pixel)
    retStream.writeInt(header.compression.toInt)
    retStream.writeInt(header.image_size_bytes.toInt)
    retStream.writeInt(header.x_resolution_ppm)
    retStream.writeInt(header.y_resolution_ppm)
    retStream.writeInt(header.num_colors.toInt)
    retStream.writeInt(header.important_colors.toInt)

    if (header.offset != 54) {
      val ajOffset = (header.offset - 54.toLong).toInt
      val ajArray  = Array.fill(ajOffset)(0.toByte)
      retStream.write(ajArray, 0, ajOffset)
    }

    // write pixels
    (0 until (header.image_size_bytes * 8 / header.bits_per_pixel).toInt).foreach { idx =>
      retStream.writeByte(0xff & pixels.b(idx))
      retStream.writeByte(0xff & pixels.g(idx))
      retStream.writeByte(0xff & pixels.r(idx))
      if (header.bits_per_pixel == 32) {
        retStream.writeByte(0xff & pixels.a(idx))
      }

    }
    retStream.close()
  }

  def getPixels(x: Int, y: Int) = {
    val ret = Array.fill(3)(0.toShort)
    var xIdx, yIdx = 0
    x match {
      case i if i > header.height_px - 1            => xIdx = header.height_px - 1
      case i if i < 0                               => xIdx = 0
      case i if i >= 0 && i <= header.height_px - 1 => xIdx = i
    }
    y match {
      case i if i > header.width_px - 1            => yIdx = header.width_px - 1
      case i if i < 0                              => yIdx = 0
      case i if i >= 0 && i <= header.width_px - 1 => yIdx = i
    }

    header.height_px >= 0 match {
      case true =>
        ret(0) = pixels.r(yIdx + header.width_px * (header.height_px - xIdx - 1))
        ret(1) = pixels.g(yIdx + header.width_px * (header.height_px - xIdx - 1))
        ret(2) = pixels.b(yIdx + header.width_px * (header.height_px - xIdx - 1))
      case false =>
        ret(0) = pixels.r(yIdx + xIdx * header.width_px)
        ret(1) = pixels.g(yIdx + xIdx * header.width_px)
        ret(2) = pixels.b(yIdx + xIdx * header.width_px)
    }
    ret
  }

  def assignPixels(x: Int, y: Int, pixel: Short, idx: Int) = {
    idx match {
      case 0 => pixels.r(transPosition(x, y)) = pixel
      case 1 => pixels.g(transPosition(x, y)) = pixel
      case 2 => pixels.b(transPosition(x, y)) = pixel
      case _ => throw new IndexOutOfBoundsException
    }

  }

  def transPosition(x: Int, y: Int) = {
    val ret = header.height_px >= 0 match {
      case true  => y + header.width_px * (header.height_px - x - 1)
      case false => y + x * header.width_px
    }
    ret
  }
}

object Test extends App {
  val a = BMPImage(3840, 2160)
  a.bmpRead("0.bmp", "/home/lqx/Sampling_src/test_bmp_4K/GT/")
  //a.bmpWrite("1_upsampling.bmp")
//  (0 until 15).foreach{x =>
//    (0 until 15).foreach{ y =>
//      println(s"the r is ${a.getPixels(x, y).r}")
//      println(s"the g is ${a.getPixels(x, y).g}")
//      println(s"the b is ${a.getPixels(x, y).b}")
//    }
//  }

}
