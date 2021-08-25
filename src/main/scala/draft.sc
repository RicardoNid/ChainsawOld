import spinal.core.internals.BaseNode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

val temp = new Array[mutable.ArrayBuffer[BaseNode]](3)
for(i <- 0 until temp.length) temp(i) = new ArrayBuffer[BaseNode]

temp