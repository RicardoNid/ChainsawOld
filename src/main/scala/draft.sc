import scala.collection.mutable.ArrayBuffer

class BasicIntQueue {
  private val buf = new ArrayBuffer[Int]()

  def get = buf.remove(0)

  def put(x: Int) = buf += x
}

var a,b,c = 0
val temp = ArrayBuffer(a,b,c)
temp(0) = 3
a
temp