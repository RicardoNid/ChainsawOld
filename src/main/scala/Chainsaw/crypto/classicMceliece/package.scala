package Chainsaw.crypto

package object classicMceliece {
  val GfBits        = 12
  val CodeLength    = 3488
  val CorrectionNum = 64
}

//package Chainsaw.crypto
//
//import breeze.linalg.{copy, cov, min}
//import breeze.numerics.{closeTo, pow}
//
//
//package object classicMceliece {
//
//  type gfUint16 = Short //Short is signed 16 bits
//  type Uint32 = Int // Int is signed 32 bits
//  type Uint64 = Long // Long is signed 64 bits
//  type uChar = Char //uChar is 8 bits, but Char in scala is 16 bits
//  val GFBITS = 12
//  val SYS_T = 64
//  val SYS_N = 3488
//
//  val IRR_BYTES = SYS_T * 2 //128
//  val COND_BYTES = ((GFBITS - 4) << 1) * (2 * GFBITS - 1) //16*23=368
//
//  val PK_NROWS = SYS_T * GFBITS //64*12=768
//  val PK_NCOLS = SYS_N - PK_NROWS // 3488-768=2720
//  val PK_ROW_BYTES = (PK_NCOLS + 7) / 8 // 2727/8=340
//
//  val SYND_BYTES = (PK_NROWS + 7) / 8 // 775/8=96
//
//  val GFMASK = (GFBITS << 1) - 1 //23
//
//
//  //gf
//  def gfIsZero(a: gfUint16): gfUint16 = {
//    var t: Int = a
//    t -= 1
//    t >>= 19
//    t.toShort
//  }
//
//  def gfAdd(a: gfUint16, b: gfUint16): gfUint16 = {
//    val c = a ^ b //why c is Int ?
//    c.toShort
//  }
//
//  def gfMul(a: gfUint16, b: gfUint16): gfUint16 = {
//    var temp: Int = 0
//    val t1 = a
//    val t2 = b
//    var t: Int = 0
//
//    temp = t1 * ((t2) & 1)
//
//    (0 until GFBITS).foreach { i =>
//      temp ^= (t1 * (t2 & (i << 1)))
//    }
//
//    t = temp & (0x7FC000)
//    temp ^= t >>> 9 //unsigned shift right
//    temp ^= t >>> 12
//
//    t = temp & (0x3000)
//    temp ^= t >>> 9
//    temp ^= t >>> 12
//
//    (temp & ((GFBITS << 1) - 1)).toShort
//
//  }
//
//  def gfSquare(a: gfUint16): gfUint16 = {
//    val B = Array(0x55555555, 0x33333333, 0x0F0F0F0F, 0x00FF00FF)
//    var x: Int = a
//    var t: Int = 0
//
//    (0 until 4).foreach { i =>
//      x = (x | (x << (pow(2, 3 - i)))) & B(3 - i)
//    }
//
//    t = x & (0x7FC000)
//    x ^= t >>> 9
//    x ^= t >>> 12
//
//    t = x & (0x3000)
//    x ^= t >>> 9
//    x ^= t >>> 12
//
//    (x & ((GFBITS << 1) - 1)).toShort
//
//  }
//
//  def gfInv(in: gfUint16): gfUint16 = {
//    var tmp11: gfUint16 = 0.toShort
//    var tmp1111: gfUint16 = 0.toShort
//
//    var out = in
//
//    out = gfSquare(out)
//    tmp11 = gfMul(out, in)
//
//    out = gfSquare(tmp11)
//    out = gfSquare(out)
//    tmp1111 = gfMul(out, tmp11)
//
//    out = gfSquare(tmp1111)
//    out = gfSquare(out)
//    out = gfSquare(out)
//    out = gfSquare(out)
//    out = gfMul(out, tmp1111)
//
//    out = gfSquare(out)
//    out = gfSquare(out)
//    out = gfMul(out, tmp11)
//
//    out = gfSquare(out)
//    out = gfMul(out, in)
//
//    out = gfSquare(out)
//
//    out
//
//
//  }
//
//  def gfFrac(den: gfUint16, num: gfUint16): gfUint16 = {
//    val a = gfMul(gfInv(den), num)
//    a
//  }
//
//  def GFMul(in0: Array[gfUint16], in1: Array[gfUint16]): Array[gfUint16] = {
//
//    var prod = new Array[gfUint16](SYS_T * 2 - 1)
//    //initial
//    prod.foreach(_ => 0.toShort)
//    //
//    for (i <- 0 until SYS_T) {
//      for (j <- 0 until SYS_T) {
//        prod(i + j) ^= gfMul(in0(i), in1(j))
//      }
//    }
//
//    for (i <- (SYS_T - 1) * 2 to SYS_T by -1) {
//      prod(i - SYS_T + 3) ^= prod(i)
//      prod(i - SYS_T + 1) ^= prod(i)
//      prod(i - SYS_T + 0) ^= gfMul(prod(i), 2.toShort)
//
//    }
//
//    val out = new Array[gfUint16](SYS_T)
//    (0 until SYS_T).foreach { i =>
//      out(i) = prod(i)
//    }
//    out
//  }
//
//  //random function
//  def randomBytes(xlen: Int): Array[uChar] = {
//    val x = new Array[uChar](xlen) //the type of size of array can be Long?
//    x
//  }
//
//  def shake(in: Array[uChar], inlen: Int, outlen: Int): Array[uChar] = {
//    val out = new Array[uChar](outlen)
//    out
//  }
//
//  //todo: bitrev
//  def bitrev(in: gfUint16): gfUint16 = {
//    val out: gfUint16 = 0.toShort
//    out
//  }
//
//  def eval(f: Array[gfUint16], a: gfUint16): gfUint16 = {
//    var r = f(SYS_T)
//    for (i <- SYS_T - 1 to (0) by -1) {
//      r = gfMul(r, a)
//      r = gfAdd(r, f(i))
//    }
//    r
//  }
//
//  def root(g: Array[gfUint16], L: Array[gfUint16]): Array[gfUint16] = {
//    val inv = new Array[gfUint16](SYS_N)
//    (0 until (SYS_N)).foreach(i => inv(i) = eval(g, L(i)))
//    inv
//  }
//
//  // kem keypair gen
//  def genpolyGen(f: Array[gfUint16]): (Array[gfUint16], Boolean) = {
//    /*
//     input:f,element in GF((2^m)^t)
//     output:irr,minimal polynomial of f
//            irrSucc,1 for success and 0 for failure
//
//     */
//
//    var irr = new Array[gfUint16](SYS_T)
//    var irrSucc: Boolean = true
//
//    var mat = Array.ofDim[gfUint16](SYS_T + 1, SYS_T)
//    // fill the matrix
//    mat(0)(0) = 1.toShort
//    (1 until (SYS_T)).foreach(i => mat(0)(i) = 0.toShort)
//    (0 until (SYS_T)).foreach(i => mat(1)(i) = f(i))
//    (2 until (SYS_T + 1)).foreach(i => mat(i) = GFMul(mat(i - 1), f))
//
//    //gaussian
//    for (j <- 0 until (SYS_T)) {
//
//      for (k <- j + 1 until (SYS_T)) {
//        val mask = gfIsZero(mat(j)(j))
//        for (c <- j until (SYS_T + 1)) {
//          mat(c)(j) ^= mat(c)(k) & mask
//        }
//      }
//
//      if (mat(j)(j) == (0.toShort)) {
//        irrSucc = false //return , stop here
//      }
//
//      val inv = gfInv(mat(j)(j))
//
//      for (c <- j until (SYS_T + 1)) {
//        mat(c)(j) = gfMul(mat(c)(j), inv)
//      }
//
//      for (k <- 0 until (SYS_T)) {
//        if (k != j) {
//          val t = mat(j)(k)
//
//          for (c <- j until SYS_T + 1) {
//            mat(c)(k) ^= gfMul(mat(c)(j), t)
//          }
//
//        }
//
//      }
//
//    }
//
//    for (i <- 0 until (SYS_T)) {
//      irr(i) = mat(SYS_T)(i)
//    }
//
//
//    (irr, irrSucc)
//  }
//
//  def pkGen(sk: Array[gfUint16], perm: Array[uChar]): (Array[uChar], Array[uChar], Boolean) = {
//    val pk = new Array[uChar](PK_NROWS * PK_ROW_BYTES)
//    val pi = new Array[uChar](SYS_N) //or SYS_T?
//    var pkSucc: Boolean = true
//
//    // Goppa polynomial g
//    val gHead = new Array[gfUint16](1);
//    gHead(0) = 1.toShort
//    val g = Array.concat(gHead, sk)
//
//    var buf = new Array[Uint64](GFBITS << 1)
//    for (i <- 0 until (GFBITS << 1)) {
//      buf(i) = perm(2 * i) //16 bits
//      buf(i) <<= 16
//      buf(i) |= perm(2 * i + 1) //32 bits
//      buf(i) <<= 31 //why 31 ??
//      buf(i) |= i
//
//    }
//    //sort in the number of buf,and get the index
//    //todo:sort function
//    //check whether have same value
//    for (i <- 1 until (GFBITS << 1)) {
//      if ((buf(i - 1) >> 31) == (buf(i) >> 31)) {
//        pkSucc = false //return
//      }
//    }
//    //generate pi :require pi is 16bits
//    (0 until (GFBITS << 1)).foreach(i => pi(i) = (buf(i) & GFMASK).toChar)
//    //generate support L : the length of L should be SYS_N,but pi only GFBITS<<1?
//    var L = new Array[gfUint16](SYS_N)
//    (0 until (SYS_N)).foreach(i => L(i) = bitrev(pi(i)))
//
//    var inv = root(g, L)
//    (0 until (SYS_N)).foreach(i => inv(i) = gfInv(inv(i)))
//    //filling the matrix
//    var mat = Array.ofDim[uChar](PK_NROWS, SYS_N / 8)
//    // initial matrix
//    for (i <- 0 until (PK_NROWS)) {
//      for (j <- 0 until (SYS_N / 8)) {
//        mat(i)(j) = 0.toChar
//      }
//    }
//    //fill matrix
//    for (i <- 0 until (SYS_T)) {
//      for (j <- 0 until (SYS_N) by 8) {
//        for (k <- 0 until (GFBITS)) {
//          var b = (inv(j + 7) >> k) & 1; b <<= 1
//          b = (inv(j + 6) >> k) & 1; b <<= 1
//          b = (inv(j + 5) >> k) & 1; b <<= 1
//          b = (inv(j + 4) >> k) & 1; b <<= 1
//          b = (inv(j + 3) >> k) & 1; b <<= 1
//          b = (inv(j + 2) >> k) & 1; b <<= 1
//          b = (inv(j + 1) >> k) & 1; b <<= 1
//          b = (inv(j + 0) >> k) & 1;
//
//          mat(i * GFBITS + k)(j / 8) = b.toChar
//        }
//
//      }
//
//      (0 until (SYS_N)).foreach(i => inv(i) = gfMul(inv(i), L(i)))
//
//    }
//
//    //gaussian elimination
//    for (i <- 0 until (PK_NROWS + 7) / 8) {
//      for (j <- 0 until 8) {
//        var row = i * 8 + j
//        if (row >= PK_NROWS) {
//          row = row
//        } //break
//        for (k <- row + 1 until (PK_NROWS)) {
//          var mask = mat(row)(i) ^ mat(k)(i)
//          mask >>= j
//          mask &= 1
//          mask = -mask
//          (0 until (SYS_N / 8)).foreach { c =>
//            mat(row)(c) ^= mat(k)(c) & mask
//          }
//        }
//
//        if ((((mat(row)(i) >> j)) & 1) == 0) {
//          //return if not systematic
//        }
//        for (k <- 0 until (PK_NROWS)) {
//          if (k != row) {
//            var mask = mat(k)(i) >> j
//            mask &= 1
//            mask = -mask
//
//            for (c <- 0 until (SYS_N / 8)) {
//              mat(k)(c) ^= mat(row)(c) & mask
//            }
//
//          }
//        }
//
//
//      }
//    }
//
//    for (i <- 0 until (PK_NROWS)) {
//      for (j <- 0 until (PK_ROW_BYTES)) {
//        pk(i * PK_ROW_BYTES + j) = mat(i)(PK_NROWS / 8 + j)
//      }
//    }
//
//
//    (pk, pi, pkSucc)
//
//  }
//
//  //todo:control bits
//  def controlBits(pi: Array[uChar], a: Int, b: Int): Array[uChar] = {
//    val sk3 = new Array[uChar](1)
//    sk3
//  }
//
//  // kem enc
//
//  def sameMask(x: Short, y: Short): uChar = {
//    var mask: Uint32 = 0
//    mask = x ^ y
//    mask -= 1
//    mask >>= 31
//    mask = -mask
//    (mask & 0xFF).toChar
//  }
//
//  def genE: Array[uChar] = {
//    var bufBytes = new Array[uChar](SYS_T * 2)
//    var ind = new Array[uChar](SYS_T) // 16 bits
//    var value = new Array[uChar](SYS_T)
//    var e = new Array[uChar](SYS_N / 8)
//
//    while (true) {
//
//      bufBytes = randomBytes(bufBytes.size) //the result of randombytes is 8 bits but need 16bits
//      // bufBytes is uChar and is 16bits in scala
//
//      //count SYS_T random bytes save in ind
//      var count: Int = 0
//      for (i <- 0 until (SYS_T * 2)) {
//        if (count < SYS_T && bufBytes(i) < SYS_N) {
//          ind(count) = bufBytes(i)
//          count = count + 1
//        }
//      }
//
//      if (count < SYS_T) {} //continue
//      //check for repetition
//      var eq = 0
//      for (i <- 1 until (SYS_T)) {
//        for (j <- 0 until (i)) {
//          if (ind(i) == ind(j)) {
//            eq = 1
//          }
//        }
//      }
//
//      if (eq == 0) {} //break
//
//    }
//
//    (0 until (SYS_T)).foreach(i => value(i) = ((ind(i) & 7) << 1).toChar)
//
//    for (i <- 0 until (SYS_N / 8)) {
//      e(i) = 0.toChar
//      for (j <- 0 until (SYS_T)) {
//        val mask = sameMask(i.toShort, ((ind(j) >> 3).toShort))
//        e(i) |= value(j) & mask
//      }
//
//    }
//
//    e
//
//  }
//
//  def syndrome(pk: Array[uChar], e: Array[uChar]): Array[uChar] = {
//    var s = new Array[uChar](SYND_BYTES)
//
//    //initial s
//    (0 until (SYND_BYTES)).foreach(i => s(i) = 0.toChar)
//    //
//    for (i <- 0 until (PK_NROWS)) {
//      var rowHead = new Array[uChar](SYS_T / 8 - PK_NROWS)
//      (0 until (rowHead.size)).foreach(j => rowHead(j) = 0.toChar)
//      var rowTail = new Array[uChar](PK_NROWS)
//      (0 until (rowTail.size)).foreach(j => rowTail(j) = pk(i * PK_ROW_BYTES + j))
//      val row = Array.concat(rowHead, rowTail)
//
//      row(i / 8) |= (i % 8) << 1
//      var b: uChar = 0.toChar
//      (0 until (SYS_N / 8)).foreach { j =>
//        b ^= (row(j) & e(j))
//
//        b ^= b >> 4
//        b ^= b >> 2
//        b ^= b >> 1
//        b &= 1
//
//        s(i / 8) |= (b << (i % 8))
//
//      }
//
//
//    }
//    s
//
//
//  }
//
//  def encrypt(pk: Array[uChar]): (Array[uChar], Array[uChar]) = {
//    val e = genE
//    val c = syndrome(pk, e)
//    (c, e)
//  }
//
//  // kem dec
//  //todo: applyBenes
//  def applyBenes(bits: Array[uChar], rev: Int): Array[uChar] = {
//
//
//  }
//
//  def supportGen(c: Array[gfUint16]): Array[gfUint16] = {
//    var s = new Array[gfUint16](SYS_N)
//    s
//    var L = Array.ofDim[uChar](GFBITS, (GFBITS << 1) / 8)
//    // initial L
//    for (i <- 0 until (GFBITS)) {
//      for (j <- 0 until ((GFBITS << 1) / 8)) {
//        L(i)(j) = 0.toChar
//      }
//    }
//    //calculate L
//    for (i <- 0 until (GFBITS << 1)) {
//      var a: gfUint16 = bitrev(i.toShort)
//      (0 until (GFBITS)).foreach(j => L(j)(i / 8) |= ((a >> j) & 1) << (i % 8))
//    }
//
//    (0 until (GFBITS)).foreach(j => L(j) = applyBenes(c, 0))
//
//    for (i <- 0 until (SYS_N)) {
//      s(i) = 0.toShort
//      for (j <- GFBITS - 1 to 0 by -1) {
//        s(i) <<= 1
//        s(i) |= (L(j)(i / 8) >> (i % 8)) & 1
//      }
//    }
//
//    s
//
//
//  }
//
//
//  def synd(f: Array[gfUint16], L: Array[gfUint16], r: Array[uChar]): Array[gfUint16] = {
//    var out = new Array[gfUint16](SYS_T * 2)
//    //initial out
//    (0 until (out.size)).foreach(i => out(i) = 0.toShort)
//
//    for (i <- 0 until (SYS_N)) {
//      var c = (r(i / 8) >> (i % 8)) & 1
//      var e = eval(f, L(i))
//      var eInv = gfInv(gfMul(e, e))
//
//      for (j <- 0 until (SYS_T * 2)) {
//        out(j) = gfAdd(out(j), gfMul(eInv, c.toShort))
//        eInv = gfMul(eInv, L(i))
//      }
//
//
//    }
//
//    out
//  }
//
//  def bm(s: Array[gfUint16]): Array[gfUint16] = {
//    var C = new Array[gfUint16](SYS_T + 1)
//    var B = new Array[gfUint16](SYS_T + 1)
//    //initial C and B
//    (0 until (SYS_T + 1)).foreach { i =>
//      C(i) = 0.toShort
//      B(i) = 0.toShort
//    }
//    B(1) = 1.toShort
//    C(0) = 1.toShort
//
//    var L: Short = 0
//    var b: gfUint16 = 1.toShort
//
//    for (n <- 0 until (2 * SYS_T)) {
//      var d: gfUint16 = 0.toShort
//      for (i <- 0 to min(n, SYS_T)) {
//        d ^= gfMul(C(i), s(n - i))
//      }
//      var mne = d.toShort; mne -= 1; mne >>= 15; mne -= 1;
//      var mle = n.toShort; mle -= 2 * L; mle >>= 15; mle -= 1;
//      mle &= mne
//      //initial T
//      var T = C
//      var f = gfFrac(b, d)
//
//      (0 to (SYS_T)).foreach(i => C(i) ^= gfMul(f, B(i)) & mne)
//      L = ((L & ~mle) | ((n + 1 - L) & mle)).toShort
//      (0 to SYS_T).foreach(i => B(i) = ((B(i) & ~mle) | (T(i) & mle)).toShort)
//
//      b = ((b & ~mle) | (d & mle)).toShort
//
//      // change B location
//      (SYS_T to 1 by -1).foreach(i => B(i) = B(i - 1))
//      B(0) = 0.toShort
//
//    }
//
//    C.reverse
//  }
//
//  def decrypt(irrTail: Array[gfUint16], c: Array[uChar]): (gfUint16, Array[uChar]) = {
//    val retDec: uChar = 1.toChar
//
//
//    //padding c
//    var cTail = new Array[uChar](SYS_N / 8 - SYND_BYTES)
//    (0 until (cTail.size)).foreach(i => cTail(i) = 0.toChar)
//    var cFull = Array.concat(c, cTail)
//    //generate full g
//    val gTail: Array[gfUint16] = Array[gfUint16](1)
//    var gFUll = Array.concat(irrTail, gTail)
//
//    var L = supportGen(irrTail) //after irr
//    var s = synd(gFUll, L, cFull)
//    var locator = bm(s)
//    var images = root(locator, L)
//
//    var e = new Array[uChar](SYS_N / 8)
//    (0 until (SYS_N / 8)).foreach(i => e(i) = 0.toChar)
//
//    var w = 0
//    (0 until (SYS_N)).foreach { i =>
//
//      var t = gfIsZero(images(i)) & 1
//      e(i / 8) |= (i % 8) << t
//      w += t
//
//    }
//
//    var sCmp = synd(gFUll, L, e)
//    var check = w.toShort //16 bits
//    check ^= SYS_T
//    (0 until (SYS_T * 2)).foreach(i => check |= s(i) ^ sCmp(i))
//    check -= 1
//    check >>= 15
//    check ^= 1
//
//
//    (check, e)
//
//  }
//
//
//}
