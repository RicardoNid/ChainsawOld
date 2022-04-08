package Chainsaw.PwlWorkspace

object cnn {
  def multiply(mat: List[Double], ker: List[Double]): Double = {
    if (mat == Nil || ker == Nil) return 0;
    val h1 = mat.head
    val h2 = ker.head
    val t1 = mat.tail
    val t2 = ker.tail
    return (h1 * h2) + multiply(t1, t2)
  }

  def dotProduct(matrix: List[List[Double]], kernel: List[List[Double]]): Double = {
    val t1 = getSize(matrix)
    val t2 = getSize(kernel)
    Dot_Product(matrix, t1.head, t1.tail.head, kernel, t2.head, t2.tail.head)
  }

  def Dot_Product(matrix: List[List[Double]], n1: Int, m1: Int, kernel: List[List[Double]], n2: Int, m2: Int): Double = {

    /*if(n1==0) {return 0;}
    if(m1==0){return 0;}
    if(n2==0) {return 0;}
    if(m2==0){return 0;}*/
    if (matrix == Nil || kernel == Nil) return 0;
    val h1 = matrix.head
    val h2 = kernel.head
    val t1 = matrix.tail
    val t2 = kernel.tail
    return multiply(h1, h2) + Dot_Product(t1, n1 - 1, m1, t2, n2 - 1, m2)
  }

  def getRowList(matrix: List[List[Double]], n1: Int, m1: Int, i1: Int, j1: Int, count: Int): List[Double] = {
    if (count == i1) {
      return matrix.head
    }
    else {
      return getRowList(matrix.tail, n1, m1, i1, j1, count + 1)
    }
  }

  def getColumnList(matrix: List[Double], m1: Int, j1: Int, count: Int): Double = {
    if (count == j1) {
      return matrix.head
    }
    else {
      return getColumnList(matrix.tail, m1, j1, count + 1)
    }
  }

  def getCellValue(matrix: List[List[Double]], n1: Int, m1: Int, i1: Int, j1: Int): Double = {
    val r_list = getRowList(matrix, n1, m1, i1, j1, 0)
    // if (r_list==Nil)return -1.0
    //else
    return getColumnList(r_list, m1, j1, 0)
  }

  def getwindow(matrix: List[List[Double]], n1: Int, m1: Int, n2: Int, m2: Int, i1: Int, j1: Int, j: Int, c_row: Int = 0, c_col: Int = 0, res: List[List[Double]], temp: List[Double]): List[List[Double]] = {
    val cell = getCellValue(matrix, n1, m1, i1, j1)
    //println(cell)
    //List.concat(temp,List(cell))

    if (c_row == n2 - 1 && c_col == m2 - 1) {

      return res ::: List((temp ::: List(cell)))

    }
    else if (c_col == m2 - 1) {
      //List.concat(res,temp)

      return getwindow(matrix, n1, m1, n2, m2, i1 + 1, j, j, c_row + 1, 0, res ::: List(temp ::: (List(cell))), List[Double]())
    }
    else {
      return getwindow(matrix, n1, m1, n2, m2, i1, j1 + 1, j, c_row, c_col + 1, res, temp ::: List(cell))
    }
  }

  def convolute(Image: List[List[Double]], kernel: List[List[Double]], imageSize: List[Int], kernelSize: List[Int]): List[List[Double]] = {
    convolute1(Image, imageSize.head, imageSize.tail.head, kernel, kernelSize.head, kernelSize.tail.head, 0, 0, 0, 0, List[List[Double]](), List[Double]())
  }

  def convolute1(matrix: List[List[Double]], n1: Int, m1: Int, kernel: List[List[Double]], n2: Int, m2: Int, r_count: Int = 0, c_count: Int = 0, i: Int = 0, j: Int = 0, res: List[List[Double]], temp: List[Double]): List[List[Double]] = {

    val res1 = getwindow(matrix, n1, m1, n2, m2, i, j, j, 0, 0, List[List[Double]](), List[Double]())
    // println(res1)
    val dp = dotProduct(res1, kernel)

    //List.concat(temp,List(dp))
    if (r_count == n1 - n2 && c_count == m1 - m2) {

      return res ::: List((temp ::: List(dp)))
    }
    else if (c_count == m1 - m2) {
      //List.concat(res,temp)

      convolute1(matrix, n1, m1, kernel, n2, m2, r_count + 1, 0, i + 1, 0, res ::: List((temp ::: List(dp))), List[Double]())
    }
    else {
      return convolute1(matrix, n1, m1, kernel, n2, m2, r_count, c_count + 1, i, j + 1, res, temp ::: List(dp))
    }
  }

  def rowactivationLayer(activationFunc: Double => Double, Row: List[Double], temp: List[Double]): List[Double] = {
    if (Row == Nil) return temp
    rowactivationLayer(activationFunc, Row.tail, temp ::: List(activationFunc(Row.head)))
  }

  def activationLayer(activationFunc: Double => Double, Image: List[List[Double]]): List[List[Double]] = {
    return activationLayer1(activationFunc, Image, List[List[Double]]())
  }

  def activationLayer1(activationFunc: Double => Double, Image: List[List[Double]], res: List[List[Double]]): List[List[Double]] = {
    if (Image == Nil) return res
    val temp = rowactivationLayer(activationFunc, Image.head, List[Double]())
    return activationLayer1(activationFunc, Image.tail, res ::: List(temp))
  }

  def Relu(x: Double): Double = {
    if (x < 0) return 0
    else return x
  }

  def LeakyRelu(x: Double): Double = {
    if (x < 0) return 0.5 * x
    else return x
  }

  def getRowSize(Image: List[List[Double]]): Int = {
    if (Image == Nil) return 0
    else return 1 + getRowSize(Image.tail)
  }

  def getColSize(temp: List[Double]): Int = {
    if (temp == Nil) return 0
    else return 1 + getColSize(temp.tail)
  }

  def getSize(Image: List[List[Double]]): List[Int] = {
    val t1 = getRowSize(Image)
    val t2 = getColSize(Image.head)
    return List(t1) ::: List(t2)
  }

  def getwindow2(Image: List[List[Double]], n1: Int, m1: Int, k: Int, i1: Int, j1: Int, i: Int, j: Int, temp: List[Double]): List[Double] = {
    val cell = getCellValue(Image, n1, m1, i1, j1)
    if (j1 == j + k - 1 && i1 == i + k - 1) {
      return temp ::: List(cell)
    }
    else if (j1 == j + k - 1) {
      return getwindow2(Image, n1, m1, k, i1 + 1, j, i, j, temp ::: List(cell))
    }
    else return getwindow2(Image, n1, m1, k, i1, j1 + 1, i, j, temp ::: List(cell))

  }

  def singlePooling(poolingFunc: List[Double] => Double, Image: List[List[Double]], k: Int): List[Double] = {
    val t1 = getSize(Image)
    return singlePooling1(poolingFunc, Image, k, t1.head, t1.tail.head, 0, 0, List[Double]())
  }

  def singlePooling1(poolingFunc: List[Double] => Double, Image: List[List[Double]], k: Int, n1: Int, m1: Int, i: Int, j: Int, temp: List[Double]): List[Double] = {
    if (j >= m1) return temp
    val t1 = getwindow2(Image, n1, m1, k, i, j, i, j, List[Double]())
    val d = poolingFunc(t1)
    /*if(j+k>=m1)
    {
      return singlePooling(poolingFunc,Image,k,n1,m1,i+k,0,temp:::List(d))
    }*/
    return singlePooling1(poolingFunc, Image, k, n1, m1, i, j + k, temp ::: List(d))
  }

  def maxPool(x: List[Double]): Double = {
    return maxPool1(x, -5000000.0)
  }

  def maxPool1(x: List[Double], max: Double): Double = {
    if (x == Nil) return max
    if (x.head > max) return maxPool1(x.tail, x.head)
    else return maxPool1(x.tail, max)

  }

  def minPool(x: List[Double]): Double = {
    return minPool1(x, 5000000.0)
  }

  def minPool1(x: List[Double], min: Double): Double = {
    if (x == Nil) return min
    if (x.head < min) return minPool1(x.tail, x.head)
    else return minPool1(x.tail, min)

  }

  def getCorrectImage(Image: List[List[Double]], count: Int, k: Int): List[List[Double]] = {
    if (count == k) return Image
    else return getCorrectImage(Image.tail, count + 1, k)
  }

  def poolingLayer(poolingFunc: List[Double] => Double, Image: List[List[Double]], k: Int): List[List[Double]] = {
    val t1 = getSize(Image)
    return multiLayerPooling1(poolingFunc, Image, k, t1.head, t1.tail.head, 0, 0, List[List[Double]](), List[Double]())
  }

  def multiLayerPooling1(poolingFunc: List[Double] => Double, Image: List[List[Double]], k: Int, n1: Int, m1: Int, i: Int, j: Int, res: List[List[Double]], temp: List[Double]): List[List[Double]] = {
    if (i >= n1 || Image == Nil) return res
    val t1 = singlePooling(poolingFunc, Image, k)
    val t2 = getCorrectImage(Image, 0, k)
    return multiLayerPooling1(poolingFunc, t2, k, n1, m1, i + k, j, res ::: List(t1), List[Double]())
  }

  def mixedLayer(Image: List[List[Double]], kernel: List[List[Double]], imageSize: List[Int], kernelSize: List[Int], activationFunc: Double => Double, poolingFunc: List[Double] => Double, k: Int): List[List[Double]] = {
    val t1 = convolute(Image, kernel, imageSize, kernelSize)
    val t2 = activationLayer(activationFunc, t1)
    return poolingLayer(poolingFunc, t2, k)
  }

  def findMax(Image: List[List[Double]], max1: Double): Double = {
    if (Image == Nil) return max1
    val m1 = maxPool(Image.head)
    if (m1 > max1) return findMax(Image.tail, m1)
    else return findMax(Image.tail, max1)
  }

  def findMin(Image: List[List[Double]], min1: Double): Double = {
    if (Image == Nil) return min1
    val m1 = minPool(Image.head)
    if (m1 < min1) return findMin(Image.tail, m1)
    else return findMin(Image.tail, min1)
  }

  def normalise(Image: List[List[Double]]): List[List[Int]] = {

    val t1 = getSize(Image)
    val max1 = findMax(Image, -5000000.0)
    val min1 = findMin(Image, 5000000.0)
    return normalise1(Image, t1.head, t1.tail.head, 0, 0, List[List[Int]](), List[Int](), max1, min1)
  }

  def normalise1(Image: List[List[Double]], n1: Int, m1: Int, i: Int, j: Int, res: List[List[Int]], temp: List[Int], max1: Double, min1: Double): List[List[Int]] = {

    val cell = getCellValue(Image, n1, m1, i, j)
    val eval = (((cell - min1) / (max1 - min1)) * 255).round.toInt
    //val t1=temp:::List(eval)
    if (i == n1 - 1 && j == m1 - 1) {
      return res ::: List(temp ::: List(eval))
    }
    else if (j == m1 - 1)
      return normalise1(Image, n1, m1, i + 1, 0, res ::: List(temp ::: List(eval)), List[Int](), max1, min1)
    else return normalise1(Image, n1, m1, i, j + 1, res, temp ::: List(eval), max1, min1)
  }

  def getSum(x: List[Double]): Double = {
    if (x == Nil) return 0.0
    else return x.head + getSum(x.tail)
  }

  def getCount(x: List[Double]): Double = {
    if (x == Nil) return 0.0
    else return 1.0 + getCount(x.tail)
  }

  def avgPool(x: List[Double]): Double = {
    val t1 = getSum(x)
    val t2 = getCount(x)
    return t1 / t2
  }

  def mulhelper(x: List[Double], w1: Double, temp: List[Double]): List[Double] = {
    if (x == Nil) return temp
    return mulhelper(x.tail, w1, temp ::: List(w1 * (x.head)))
  }

  def mul(m1: List[List[Double]], w1: Double, res: List[List[Double]]): List[List[Double]] = {
    if (m1 == Nil) return res
    val t1 = mulhelper(m1.head, w1, List[Double]())
    return mul(m1.tail, w1, res ::: List(t1))
  }

  def addMatricesHelper(t1: List[Double], t2: List[Double], b: Double, temp: List[Double]): List[Double] = {
    if (t1 == Nil || t2 == Nil) return temp
    return addMatricesHelper(t1.tail, t2.tail, b, temp ::: List(t1.head + t2.head + b))
  }

  def addMatrices(t1: List[List[Double]], t2: List[List[Double]], b: Double, res: List[List[Double]]): List[List[Double]] = {
    if (t1 == Nil || t2 == Nil) return res
    val m1 = addMatricesHelper(t1.head, t2.head, b, List[Double]())
    return addMatrices(t1.tail, t2.tail, b, res ::: List(m1))
  }

  def assembly(Image: List[List[Double]], imageSize: List[Int], w1: Double, w2: Double, b: Double, Kernel1: List[List[Double]], kernelSize1: List[Int], Kernel2: List[List[Double]], kernelSize2: List[Int], Kernel3: List[List[Double]], kernelSize3: List[Int], Size: Int): List[List[Int]] = {
    val to1 = mixedLayer(Image, Kernel1, imageSize, kernelSize1, Relu, avgPool, Size)
    val to2 = mixedLayer(Image, Kernel2, imageSize, kernelSize2, Relu, avgPool, Size)
    val t1 = mul(to1, w1, List[List[Double]]())
    val t2 = mul(to2, w2, List[List[Double]]())
    val to3 = addMatrices(t1, t2, b, List[List[Double]]())
    val to4 = mixedLayer(to3, Kernel3, getSize(to3), kernelSize3, LeakyRelu, maxPool, Size)
    return normalise(to4)
  }

  def main(args: Array[String]) {
    //    val matrix = List(List(0.1, 0.4, 0.5), List(0.2, 0.8, 0.1), List(0.6, 0.9, 0.45))
    //    val matrix = List(List(12.0, 20.0, 30.0, 0.0), List(8.0, 12.0, 2.0, 0.0), List(34.0, 70.0, 37.0, 4.0), List(112.0, 100.0, 25.0, 12.0))
    //    val matrix = List(List(1.0, -1.0, 1.0, 0.0, 0), List(0.0, -1.0, 1.0, 1, 0), List(0, 0, 1.0, 1.0, 1.0), List(0, 0, 1.0, 1.0, 0.0), List(0, 1, 1.0, 0, 0.0))
    //    val kernel = List(List(1.0, 0.0, 1.0), List(0.0, 1.0, 0.0), List(1.0, 0.0, 1.0))
    //    val result = Dot_Product(matrix, 3, 3, kernel, 3, 3)
    //
    //    println(result)
    //    val res = List[List[Double]]()
    //    val temp = List[Double]()
    //    println("Hi")
    //    println(convolute(matrix, kernel, List(5) ::: List(5), List(3) ::: List(3)))
    //    println(activationLayer(Relu, matrix))
    //    println(multiLayerPooling(maxPool, matrix, 2))
    //    println(normalise(matrix))

  }

}
